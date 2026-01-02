package workhours

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.util.Try
import scala.scalajs.js

final class App // companion class so Scala can emit a static forwarder App.main

object App:

  private val exampleInput: String =
    """IN 17 déc. 2025 à 19:25
      |OUT 18 déc. 2025 à 09:01
      |IN 21 déc. 2025 à 19:56
      |OUT 22 déc. 2025 à 08:15
      |
      |IN 02 janv. 2026 à 07:30
      |OUT 02 janv. 2026 à 19:30
      |""".stripMargin

  private def hhmm(t: WorkHoursAnalyzer.Time): String =
    f"${t.hour}%02d:${t.minute}%02d"

  private def hhmm(dt: WorkHoursAnalyzer.DateTime): String =
    f"${dt.getHour}%02d:${dt.getMinute}%02d"

  private def monthName(m: Int): String =
    m match
      case 1  => "Jan"
      case 2  => "Feb"
      case 3  => "Mar"
      case 4  => "Apr"
      case 5  => "May"
      case 6  => "Jun"
      case 7  => "Jul"
      case 8  => "Aug"
      case 9  => "Sep"
      case 10 => "Oct"
      case 11 => "Nov"
      case 12 => "Dec"
      case _  => m.toString

  private def timeToMinutes(t: WorkHoursAnalyzer.Time): Int =
    t.hour * 60 + t.minute

  private def minutesToTime(mins: Int): WorkHoursAnalyzer.Time =
    WorkHoursAnalyzer.Time.fromMinutes(mins)

  private def parseInput(text: String): Either[String, List[WorkHoursAnalyzer.MonthlyWorkReport]] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    if lines.isEmpty then Left("Paste your IN/OUT lines first.")
    else
      Try(WorkHoursAnalyzer.MonthlyWorkReport.parse(lines)).toEither.left.map { e =>
        s"Parse error: ${e.getMessage}"
      }.map { reports =>
        reports.sortBy(_.month)
      }

  private def csvDataUri(csv: String): String =
    "data:text/csv;charset=utf-8," + js.URIUtils.encodeURIComponent(csv)

  private def perMonthSummaryCard(reports: List[WorkHoursAnalyzer.MonthlyWorkReport]): HtmlElement =
    val overallWorkMins = reports.map(r => timeToMinutes(r.totalWorkTime)).sum
    val overallAddMins  = reports.map(r => timeToMinutes(r.totalAdditionalTime)).sum
    val overallShifts   = reports.map(_.shifts.length).sum

    div(
      cls := "card",

      div(
        cls := "kpis",
        div(cls := "kpi", div(cls := "label", "Months"), div(cls := "value", reports.length.toString)),
        div(cls := "kpi", div(cls := "label", "Total work (all)"), div(cls := "value", hhmm(minutesToTime(overallWorkMins)))),
        div(cls := "kpi", div(cls := "label", "Additional (all)"), div(cls := "value", hhmm(minutesToTime(overallAddMins))))
      ),

      div(cls := "muted", marginTop := "10px", s"$overallShifts shift(s) total"),

      div(marginTop := "14px",
        table(
          thead(
            tr(
              th("Month"),
              th("Shifts"),
              th("Total work"),
              th("Additional")
            )
          ),
          tbody(
            reports.map { r =>
              tr(
                td(f"${monthName(r.month)} (${r.month})"),
                td(r.shifts.length.toString),
                td(hhmm(r.totalWorkTime)),
                td(hhmm(r.totalAdditionalTime))
              )
            }
          ),
          // Overall row
          tfoot(
            tr(
              td(b("All months")),
              td(b(overallShifts.toString)),
              td(b(hhmm(minutesToTime(overallWorkMins)))),
              td(b(hhmm(minutesToTime(overallAddMins))))
            )
          )
        )
      )
    )

  private def oneMonthReportCard(report: WorkHoursAnalyzer.MonthlyWorkReport): HtmlElement =
    val total = report.totalWorkTime
    val add   = report.totalAdditionalTime
    val csv   = report.generateCSV()

    div(
      cls := "card",

      div(
        cls := "kpis",
        div(cls := "kpi", div(cls := "label", "Month"), div(cls := "value", f"${monthName(report.month)} (${report.month})")),
        div(cls := "kpi", div(cls := "label", "Total work"), div(cls := "value", hhmm(total))),
        div(cls := "kpi", div(cls := "label", "Additional"), div(cls := "value", hhmm(add)))
      ),

      div(cls := "muted", marginTop := "10px", s"${report.shifts.length} shift(s)"),

      div(
        cls := "row",
        marginTop := "12px",
        a(
          cls := "button",
          href := csvDataUri(csv),
          download := f"work-report-${report.month}%02d.csv",
          "Download CSV"
        ),
        button(
          "Copy CSV",
          onClick --> { _ =>
            dom.window.navigator.clipboard.writeText(csv)
            ()
          }
        )
      ),

      div(marginTop := "14px",
        table(
          thead(
            tr(
              th("Day"),
              th("Type"),
              th("Start"),
              th("Break start"),
              th("Break end"),
              th("End"),
              th("Work"),
              th("Additional")
            )
          ),
          tbody(
            report.shifts.map { s =>
              val work = s.totalWorkDuration
              val addt = s.additionalTime()
              tr(
                td(s.dayOfMonth.toString),
                td(s.typ.toString),
                td(hhmm(s.startTime)),
                td(s.breakStartTime.map(hhmm).getOrElse("")),
                td(s.breakEndTime.map(hhmm).getOrElse("")),
                td(hhmm(s.endTime)),
                td(hhmm(work)),
                td(hhmm(addt))
              )
            }
          )
        )
      )
    )

  private def reportsView(reports: List[WorkHoursAnalyzer.MonthlyWorkReport]): HtmlElement =
    div(
      // Top summary (per-month totals + overall)
      perMonthSummaryCard(reports),

      // One card per month (table + CSV actions)
      div(
        marginTop := "12px",
        reports.map { r =>
          div(marginTop := "12px", oneMonthReportCard(r))
        }
      )
    )

  def main(args: Array[String]): Unit =
    val inputVar  = Var[String]("")
    val resultVar = Var[Either[String, List[WorkHoursAnalyzer.MonthlyWorkReport]]](
      Left("Paste your input and click “Generate report”.")
    )

    val appEl =
      div(
        cls := "container",
        h1("Work Hours Analyzer"),
        div(cls := "muted", "Paste your IN/OUT lines below (you can mix multiple months)."),

        div(
          cls := "grid",
          div(
            cls := "card",
            textArea(
              placeholder := "IN 17 déc. 2025 à 19:25\nOUT 18 déc. 2025 à 09:01\n...",
              value <-- inputVar.signal,
              onInput.mapToValue --> inputVar
            ),
            div(cls := "row", marginTop := "10px",
              button(
                "Generate report",
                onClick --> { _ => resultVar.set(parseInput(inputVar.now())) }
              ),
              button(
                "Load example",
                onClick --> { _ =>
                  inputVar.set(exampleInput)
                  resultVar.set(parseInput(exampleInput))
                }
              )
            )
          ),
          div(
            child <-- resultVar.signal.map {
              case Left(msg) =>
                div(cls := "card",
                  div(cls := "muted", "Status"),
                  div(marginTop := "10px", cls := "error", msg)
                )
              case Right(reports) =>
                if reports.isEmpty then
                  div(cls := "card",
                    div(cls := "muted", "Status"),
                    div(marginTop := "10px", cls := "error", "No report produced (no shifts parsed).")
                  )
                else
                  reportsView(reports)
            }
          )
        )
      )

    renderOnDomContentLoaded(
      dom.document.querySelector("#appContainer"),
      appEl
    )
