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
      |IN 22 déc. 2025 à 19:22
      |OUT 23 déc. 2025 à 09:17
      |""".stripMargin

  private def hhmm(t: WorkHoursAnalyzer.Time): String =
    f"${t.hour}%02d:${t.minute}%02d"

  private def hhmm(dt: WorkHoursAnalyzer.DateTime): String =
    f"${dt.getHour}%02d:${dt.getMinute}%02d"

  private def parseInput(text: String): Either[String, WorkHoursAnalyzer.MonthlyWorkReport] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    if lines.isEmpty then Left("Paste your IN/OUT lines first.")
    else
      Try(WorkHoursAnalyzer.MonthlyWorkReport.parse(lines)).toEither.left.map { e =>
        // You can enrich this later with line numbers / better diagnostics
        s"Parse error: ${e.getMessage}"
      }

  private def csvDataUri(csv: String): String =
    "data:text/csv;charset=utf-8," + js.URIUtils.encodeURIComponent(csv)

  private def reportView(report: WorkHoursAnalyzer.MonthlyWorkReport): HtmlElement =
    val total = report.totalWorkTime
    val add   = report.totalAdditionalTime
    val csv   = report.generateCSV()

    div(
      cls := "card",
      div(
        cls := "kpis",
        div(cls := "kpi", div(cls := "label", "Month"), div(cls := "value", report.month.toString)),
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
            // Clipboard requires https or localhost in most browsers
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

  def main(args: Array[String]): Unit =
    val inputVar  = Var[String]("")
    val resultVar = Var[Either[String, WorkHoursAnalyzer.MonthlyWorkReport]](
      Left("Paste your input and click “Generate report”.")
    )

    val appEl =
      div(
        cls := "container",
        h1("Work Hours Analyzer"),
        div(cls := "muted", "Paste your IN/OUT lines below (directly from your note)."),

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
              case Right(report) =>
                reportView(report)
            }
          )
        )
      )

    renderOnDomContentLoaded(
      dom.document.querySelector("#appContainer"),
      appEl
    )
