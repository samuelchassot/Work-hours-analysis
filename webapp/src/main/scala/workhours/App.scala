package workhours

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.util.Try
import scala.scalajs.js

final class App // companion class so Scala can emit a static forwarder App.main

object App:

  private val GITHUB_ISSUES_URL: String =
    "https://github.com/samuelchassot/Work-hours-analysis/issues"

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
      }.map(_.sortBy(_.month))

  private def csvDataUri(csv: String): String =
    "data:text/csv;charset=utf-8," + js.URIUtils.encodeURIComponent(csv)

  // -------- Official duration parsing (HH:MM) --------

  private def parseHHMM(s: String): Option[WorkHoursAnalyzer.Time] =
    val trimmed = s.trim
    val parts = trimmed.split(":")
    if parts.length != 2 then None
    else
      val hOpt = Try(parts(0).trim.toInt).toOption
      val mOpt = Try(parts(1).trim.toInt).toOption
      (hOpt, mOpt) match
        case (Some(h), Some(m)) if 0 <= h && h <= 24 && 0 <= m && m < 60 =>
          Some(WorkHoursAnalyzer.Time(h, m))
        case _ => None

  // -------- Views (depend on official map) --------

  private def perMonthSummaryCard(
    reports: List[WorkHoursAnalyzer.MonthlyWorkReport],
    official: Map[WorkHoursAnalyzer.ShiftType, WorkHoursAnalyzer.Time]
  ): HtmlElement =
    val overallWorkMins = reports.map(r => timeToMinutes(r.totalWorkTime)).sum
    val overallAddMins  = reports.map(r => timeToMinutes(r.totalAdditionalTime(official))).sum
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
                td(hhmm(r.totalAdditionalTime(official)))
              )
            }
          ),
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

  private def oneMonthReportCard(
    report: WorkHoursAnalyzer.MonthlyWorkReport,
    official: Map[WorkHoursAnalyzer.ShiftType, WorkHoursAnalyzer.Time]
  ): HtmlElement =
    val total = report.totalWorkTime
    val add   = report.totalAdditionalTime(official)
    val csv   = report.generateCSV(official)

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
              val addt = s.additionalTime(official)
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

  private def reportsView(
    reports: List[WorkHoursAnalyzer.MonthlyWorkReport],
    official: Map[WorkHoursAnalyzer.ShiftType, WorkHoursAnalyzer.Time]
  ): HtmlElement =
    div(
      perMonthSummaryCard(reports, official),
      div(
        marginTop := "12px",
        reports.map { r =>
          div(marginTop := "12px", oneMonthReportCard(r, official))
        }
      )
    )

  private def footer: HtmlElement =
    val year = "2026"

    div(
      cls := "muted",
      marginTop := "18px",
      fontSize := "13px",

      div(
        "If you encountered a bug or want to request a feature, open a new issue on ",
        a(
          href := GITHUB_ISSUES_URL,
          target := "_blank",
          rel := "noopener noreferrer",
          "GitHub"
        ),
        "."
      ),

      div(marginTop := "6px", s"© $year Samuel Chassot. All rights reserved.")
    )


  // -------- Official durations UI (generic) --------

  private def officialDurationsCard(
    shiftTypes: List[WorkHoursAnalyzer.ShiftType],
    defaultDurations: Map[WorkHoursAnalyzer.ShiftType, WorkHoursAnalyzer.Time],
    enabledVars: Map[WorkHoursAnalyzer.ShiftType, Var[Boolean]],
    durationTextVars: Map[WorkHoursAnalyzer.ShiftType, Var[String]],
    errorVars: Map[WorkHoursAnalyzer.ShiftType, Var[Option[String]]],
    officialMapVar: Var[Map[WorkHoursAnalyzer.ShiftType, WorkHoursAnalyzer.Time]]
  ): HtmlElement =

    def defaultFor(t: WorkHoursAnalyzer.ShiftType): WorkHoursAnalyzer.Time =
      defaultDurations.getOrElse(t, WorkHoursAnalyzer.Time(8, 0)) // safe fallback

    def recomputeOfficialMap(): Unit =
      val m = shiftTypes.flatMap { t =>
        if enabledVars(t).now() then
          val txt = durationTextVars(t).now()
          val parsedOrDefault = parseHHMM(txt).getOrElse(defaultFor(t))
          Some(t -> parsedOrDefault)
        else None
      }.toMap
      officialMapVar.set(m)

    div(
      cls := "card",
      div(cls := "muted", "Official shift durations (used to compute “Additional”)."),
      div(
        cls := "muted",
        marginTop := "6px",
        "Enable a type to override its official duration. Format: HH:MM. “Reset to default” restores the analyzer default for that type."
      ),

      div(marginTop := "12px",
        shiftTypes.map { t =>
          val enVar   = enabledVars(t)
          val txtVar  = durationTextVars(t)
          val errVar  = errorVars(t)
          val defStr  = hhmm(defaultFor(t))

          div(
            marginTop := "10px",

            div(
              cls := "row",

              label(
                cls := "muted",
                input(
                  typ := "checkbox",
                  checked <-- enVar.signal,
                  onInput.mapToChecked --> { checked =>
                    enVar.set(checked)
                    recomputeOfficialMap()
                  }
                ),
                span(marginLeft := "8px", t.toString),
                span(marginLeft := "8px", cls := "muted", s"(default $defStr)")
              ),

              input(
                typ := "text",
                width := "110px",
                placeholder := "HH:MM",
                value <-- txtVar.signal,
                disabled <-- enVar.signal.map(en => !en),
                onInput.mapToValue --> { s =>
                  txtVar.set(s)
                  if s.trim.isEmpty then
                    errVar.set(Some("Required (HH:MM)"))
                  else if parseHHMM(s).isEmpty then
                    errVar.set(Some("Invalid (use HH:MM)"))
                  else
                    errVar.set(None)

                  recomputeOfficialMap()
                }
              ),

              button(
                "Reset to default",
                disabled <-- enVar.signal.map(en => !en),
                onClick --> { _ =>
                  txtVar.set(defStr)
                  errVar.set(None)
                  recomputeOfficialMap()
                }
              )
            ),

            child <-- Signal.combine(enVar.signal, errVar.signal).map {
              case (false, _) => emptyNode
              case (true, Some(msg)) => div(cls := "error", marginTop := "6px", msg)
              case (true, None) => emptyNode
            }
          )
        }
      )
    )

  def main(args: Array[String]): Unit =
    val shiftTypes = WorkHoursAnalyzer.ShiftType.getAllTypes()
    val defaultDurations = WorkHoursAnalyzer.ShiftType.defaultOfficialDuration()

    val enabledVars: Map[WorkHoursAnalyzer.ShiftType, Var[Boolean]] =
      shiftTypes.map(t => t -> Var(false)).toMap

    // Initialize each type's text box to its analyzer default (fallback to 08:00 if absent)
    def defaultFor(t: WorkHoursAnalyzer.ShiftType): WorkHoursAnalyzer.Time =
      defaultDurations.getOrElse(t, WorkHoursAnalyzer.Time(8, 0))

    val durationTextVars: Map[WorkHoursAnalyzer.ShiftType, Var[String]] =
      shiftTypes.map(t => t -> Var(hhmm(defaultFor(t)))).toMap

    val errorVars: Map[WorkHoursAnalyzer.ShiftType, Var[Option[String]]] =
      shiftTypes.map(t => t -> Var[Option[String]](None)).toMap

    val officialMapVar =
      Var[Map[WorkHoursAnalyzer.ShiftType, WorkHoursAnalyzer.Time]](Map.empty)

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

          // LEFT column
          div(
            officialDurationsCard(
              shiftTypes = shiftTypes,
              defaultDurations = defaultDurations,
              enabledVars = enabledVars,
              durationTextVars = durationTextVars,
              errorVars = errorVars,
              officialMapVar = officialMapVar
            ),

            div(
              marginTop := "12px",
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
            )
          ),

          // RIGHT column (depends on parse results AND official durations)
          div(
            child <-- Signal.combine(resultVar.signal, officialMapVar.signal).map {
              case (Left(msg), _) =>
                div(cls := "card",
                  div(cls := "muted", "Status"),
                  div(marginTop := "10px", cls := "error", msg)
                )

              case (Right(reports), official) =>
                if reports.isEmpty then
                  div(cls := "card",
                    div(cls := "muted", "Status"),
                    div(marginTop := "10px", cls := "error", "No report produced (no shifts parsed).")
                  )
                else
                  reportsView(reports, official)
            }
          )
        ),

        footer
      )

    renderOnDomContentLoaded(
      dom.document.querySelector("#appContainer"),
      appEl
    )
