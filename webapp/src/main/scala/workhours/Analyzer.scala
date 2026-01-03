package workhours

object WorkHoursAnalyzer:

  case class Date(day: Int):
    require(1 <= day && day <= 32, "Day must be between 1 and 32") // 32 is used when a night shift spans two months
  case class Time(hour: Int, minute: Int):
    // require(0 <= hour && hour < 24, "Hour must be between 0 and 23")
    require(0 <= minute && minute < 60, "Minute must be between 0 and 59")

    infix def > (other: Time): Boolean =
      if this.hour != other.hour then
        this.hour > other.hour
      else
        this.minute > other.minute

    infix def + (other: Time): Time =
      val totalTimeInMinutes = (this.hour + other.hour) * 60 + (this.minute + other.minute)
      Time.fromMinutes(totalTimeInMinutes)

    infix def - (other: Time): Time =
      require(this > other, "Cannot subtract a larger Time from a smaller Time")
      val thisTotalMinutes = this.hour * 60 + this.minute
      val otherTotalMinutes = other.hour * 60 + other.minute
      val diffMinutes = thisTotalMinutes - otherTotalMinutes
      Time.fromMinutes(diffMinutes)
    
    def toDoubleHours: Double =
      hour + minute.toDouble / 60.0

  end Time

  object Time:
    def fromMinutes(totalMinutes: Int): Time =
      val hours = totalMinutes / 60
      val minutes = totalMinutes % 60
      Time(hours, minutes)
  case class DateTime(date: Date, time: Time):
    require(time.hour >= 0 && time.hour < 24, "Hour must be between 0 and 23 in DateTime")
    def getDayOfMonth: Int = date.day
    def getHour: Int = time.hour
    def getMinute: Int = time.minute

    infix def < (other: DateTime): Boolean =
      if this.date.day != other.date.day then
        this.date.day < other.date.day
      else if this.time.hour != other.time.hour then
        this.time.hour < other.time.hour
      else
        this.time.minute < other.time.minute
  end DateTime

  object DateTime:
    def timeDifferenceMinutes(from: DateTime, to: DateTime): Int = 
      require(from < to, "The 'from' DateTime must happen before the 'to' DateTime")
      val minutesSinceStartOfMonthFrom = from.date.day * 24 * 60 + from.time.hour * 60 + from.time.minute
      val minutesSinceStartOfMonthTo = to.date.day * 24 * 60 + to.time.hour * 60 + to.time.minute
      minutesSinceStartOfMonthTo - minutesSinceStartOfMonthFrom


  enum ShiftType:
    case Day
    case Night

  object ShiftType:
    def getAllTypes(): List[ShiftType] = List(ShiftType.Day, ShiftType.Night)
    def defaultOfficialDuration(): Map[ShiftType, Time] =
      Map(
        ShiftType.Day -> Time(11, 0),
        ShiftType.Night -> Time(12, 0)
      )
  
  /**
    * Represents a work shift with start and end times, including optional break times.
    * The day of the shift is determined by its start time.
    * A shift can span 2 days (e.g., night shift) but should not span more than 24 hours.
    *
    * @param typ
    * @param startTime
    * @param breakStartTime
    * @param breakEndTime
    * @param endTime
    */
  case class Shift(typ: ShiftType, startTime: DateTime, breakStartTime: Option[DateTime], breakEndTime: Option[DateTime], endTime: DateTime):
    require(startTime < endTime, "Shift start time must be before end time")
    require((breakStartTime.isEmpty && breakEndTime.isEmpty) || (breakStartTime.isDefined && breakEndTime.isDefined), "Both break start and end times must be defined or both must be empty")
    require(breakStartTime.forall(bs => startTime < bs && bs < endTime), "Break start time must be within shift times")
    require(breakEndTime.forall(be => startTime < be && be < endTime), "Break end time must be within shift times")
    require(breakStartTime.zip(breakEndTime).forall{ case (bs, be) => bs < be }, "Break start time must be before break end time")
    require(totalWorkDuration.hour * 60 + totalWorkDuration.minute <= 24 * 60, "Total work duration must not exceed 24 hours")

    // We assume that shifts do not span more than 24 hours
    // We define that the day of a shift is determined by its start time
    def dayOfMonth: Int = startTime.getDayOfMonth

    /**
      * Return the total work duration of the shift, excluding breaks
      * in hours and minutes.
      *
      * @return
      */
    def totalWorkDuration: Time =
      val totalDurationMinutes = DateTime.timeDifferenceMinutes(startTime, endTime)
      val breakDurationMinutes = (breakStartTime, breakEndTime) match
        case (Some(bs), Some(be)) => DateTime.timeDifferenceMinutes(bs, be)
        case _ => 0
      val workDurationMinutes = totalDurationMinutes - breakDurationMinutes
      Time.fromMinutes(workDurationMinutes)

    infix def < (other: Shift): Boolean =
      this.startTime < other.startTime

    /**
     * Calculate additional time worked beyond official shift duration.
     * If the shift official duration is not provided for the shift type, defaults to 8 hours.
     * @return
     */
    def additionalTime(shiftOfficialDurationTime: Map[ShiftType, Time] = Map()): Time = 
      if totalWorkDuration > shiftOfficialDurationTime.getOrElse(typ, Time(8, 0)) then
        totalWorkDuration - shiftOfficialDurationTime.getOrElse(typ, Time(8, 0))
      else
        Time(0, 0)

  end Shift
  object Shift:
    /**
      * Parse from lines of text representing a shift.
      * 
      * Example format:
        IN 17 déc. 2025 à 07:30
        OUT 17 déc. 2025 à 12:00
        IN 17 déc. 2025 à 12:45
        OUT 17 déc. 2025 à 19:45
      * 
      * Night shift example:
        IN 17 déc. 2025 à 21:00
        OUT 18 déc. 2025 à 06:00
      * 
      * When only two lines are provided, no break is assumed.
      *
      * @param ls
      * @return
      */
    def parse(ls: List[String]): Option[Shift] = 
      require(ls.length == 2 || ls.length == 4, "Shift must be represented by 2 or 4 lines")
      def parseDateTime(line: String): DateTime =
        // Example line: IN 17 déc. 2025 à 07:30
        val parts = line.split(" ")
        val day = parts(1).toInt
        val timeParts = parts(5).split(":")
        val hour = timeParts(0).toInt
        val minute = timeParts(1).toInt
        DateTime(Date(day), Time(hour, minute))
      ls match
        case start :: breakStart :: breakEnd :: endLine :: Nil =>
          assert(start.startsWith("IN") && breakStart.startsWith("OUT") && breakEnd.startsWith("IN") && endLine.startsWith("OUT"))
          val startTime = parseDateTime(start)
          val breakStartTime = parseDateTime(breakStart)
          val breakEndTime = parseDateTime(breakEnd)
          val tempEndTime = parseDateTime(endLine)
          val typ = if startTime.getHour >= 16 then ShiftType.Night else ShiftType.Day
          
          val endTime = if startTime.getDayOfMonth > tempEndTime.getDayOfMonth then
            assert(tempEndTime.getDayOfMonth == 1, "End time day must be 1 when start time day is greater than end time day, i.e., the shift spans two months")
            DateTime(Date(startTime.getDayOfMonth + 1), tempEndTime.time)
          else
            tempEndTime

          if DateTime.timeDifferenceMinutes(startTime, endTime) > 24 * 60 then
            None
          else
            assert(if typ == ShiftType.Night then endTime.getDayOfMonth == startTime.getDayOfMonth + 1 else true)
            Some(Shift(typ, startTime, Some(breakStartTime), Some(breakEndTime), endTime))
        case start :: endLine :: Nil =>
          assert(start.startsWith("IN") && endLine.startsWith("OUT"))
          val startTime = parseDateTime(start)
          val tempEndTime = parseDateTime(endLine)
          val typ = if startTime.getHour >= 16 then ShiftType.Night else ShiftType.Day

          val endTime = if startTime.getDayOfMonth > tempEndTime.getDayOfMonth then
            assert(tempEndTime.getDayOfMonth == 1, "End time day must be 1 when start time day is greater than end time day, i.e., the shift spans two months")
            DateTime(Date(startTime.getDayOfMonth + 1), tempEndTime.time)
          else
            tempEndTime

          if DateTime.timeDifferenceMinutes(startTime, endTime) > 24 * 60 then
            None
          else
            assert(if typ == ShiftType.Night then endTime.getDayOfMonth == startTime.getDayOfMonth + 1 else true)
            Some(Shift(typ, startTime, None, None, endTime))
        case _ => None

  
  case class MonthlyWorkReport(month: Int, shifts: List[Shift]):
    given ord: Ordering[Shift] = Ordering.fromLessThan(_ < _)
    require(1 <= month && month <= 12, "Month must be between 1 and 12")
    require(shifts.sorted == shifts, "Shifts must be sorted by start time")
    /**
      * Calculate the total work hours and minutes for the month.
      *
      * @return
      */
    def totalWorkTime: Time =
      val totalMinutes = shifts.map { shift =>
        val time = shift.totalWorkDuration
        time.hour * 60 + time.minute
      }.sum
      Time.fromMinutes(totalMinutes)

    
    def totalAdditionalTime(shiftOfficialDurationTime: Map[ShiftType, Time] = Map()): Time = 
      shifts.map(_.additionalTime(shiftOfficialDurationTime)).foldLeft(Time(0,0))(_ + _)

    /**
     * Generate a CSV report of the shifts.
      * 
      * Each line contains:
      * Day of month, Shift type (Day/Night), Start time, Break start time, Break end time, End time, Total work duration
      * 
      *
      * @return
      */
    def generateCSV(shiftOfficialDurationTime: Map[ShiftType, Time] = Map()): String = 
      val header = "Month,Day,Shift Type,Start Time,Break Start Time,Break End Time,End Time,Total Work Duration,Additional Time\n"
      val lines = shifts.map { shift =>
        val workTime = shift.totalWorkDuration
        val breakStartStr = shift.breakStartTime.map(dt => f"${dt.getHour}%02d:${dt.getMinute}%02d").getOrElse("")
        val breakEndStr = shift.breakEndTime.map(dt => f"${dt.getHour}%02d:${dt.getMinute}%02d").getOrElse("")
        val addTime = shift.additionalTime(shiftOfficialDurationTime)
        s"${month},${shift.dayOfMonth},${shift.typ},${f"${shift.startTime.getHour}%02d:${shift.startTime.getMinute}%02d"},$breakStartStr,$breakEndStr,${f"${shift.endTime.getHour}%02d:${shift.endTime.getMinute}%02d"},${f"${workTime.hour}%02d:${workTime.minute}%02d"},${f"${addTime.hour}%02d:${addTime.minute}%02d"}"
      }
      header + lines.mkString("\n")

    def ++(other: MonthlyWorkReport): MonthlyWorkReport =
      require(this.month == other.month, "Cannot combine MonthlyWorkReports of different months")
      require((this.shifts.isEmpty || other.shifts.isEmpty) || this.shifts.last.startTime < other.shifts.head.startTime, "Shifts in the two MonthlyWorkReports must not overlap in days")
      MonthlyWorkReport(this.month, this.shifts ++ other.shifts)

    def append(shift: Shift): MonthlyWorkReport =
      require((if shifts.isEmpty then true else shifts.last < shift), "New shift must start after the last shift in the report")
      MonthlyWorkReport(this.month, this.shifts :+ shift)
  end MonthlyWorkReport

  object MonthlyWorkReport:
    /**
      * parse from one line IN 17 déc. 2025 à 07:30
      *
      * @param l
      * @return
      */
    def parseMonth(l: String): Int =
      
      val parts = l.split(" ")
      val monthStr = parts(2)
      monthStr match
        case "janv." => 1
        case "févr." => 2
        case "mars"  => 3
        case "avr."  => 4
        case "mai"   => 5
        case "juin"  => 6
        case "juil." => 7
        case "août"  => 8
        case "sept." => 9
        case "oct."  => 10
        case "nov."  => 11
        case "déc."  => 12
        case _       => throw new IllegalArgumentException(s"Unknown month: $monthStr")

    
    private def parse(ls: List[String], acc: MonthlyWorkReport): MonthlyWorkReport =
      require(ls.forall(line => line.startsWith("IN") || line.startsWith("OUT")), "All lines must start with IN or OUT")
      require(ls.forall(line =>parseMonth(line) == acc.month), "All lines must belong to the same month")
      // Algo: if more than 4 lines left, try to parse 4 lines, if fails, try to parse 2 lines
      ls match
        case l1 :: l2 :: l3 :: l4 :: tail => 
          Shift.parse(List(l1, l2, l3, l4)) match
            case Some(shift) =>
              parse(tail, acc.append(shift))
            case None =>
              // Try parsing 2 lines
              Shift.parse(List(l1, l2)) match
                case Some(shift2) =>
                  parse(l3 :: l4 :: tail, acc.append(shift2))
                case None =>
                  throw new IllegalArgumentException("Failed to parse shift from lines")
        case l1 :: l2 :: Nil =>
          Shift.parse(List(l1, l2)) match
            case Some(shift) =>
              acc.append(shift)
            case None =>
              throw new IllegalArgumentException("Failed to parse shift from lines")

        case Nil => acc

        case _ => throw new IllegalArgumentException("Invalid number of lines to parse shifts")


    /**
      * Parse a list of lines representing multiple shifts into a MonthlyWorkReport.
      * Each shift is represented by 2 or 4 lines.
      * Shifts are not separated by anything
      * 
      * Example input:
      * IN 17 déc. 2025 à 07:30
      * OUT 17 déc. 2025 à 12:00
      * IN 17 déc. 2025 à 12:45
      * OUT 17 déc. 2025 à 19:45
      * IN 18 déc. 2025 à 19:30
      * OUT 19 déc. 2025 à 07:30
      * IN 21 déc. 2025 à 07:30
      * OUT 21 déc. 2025 à 12:12
      * IN 21 déc. 2025 à 12:20
      * OUT 21 déc. 2025 à 19:30
      *
      * @param ls
      * @return
      */
    def parse(ls: List[String]): List[MonthlyWorkReport] =
      require(ls.nonEmpty, "Input lines cannot be empty")
      val months = ls.map(parseMonth).distinct
      val month = parseMonth(ls.head)
      months.map(m => 
        val monthLines = ls.filter(line => parseMonth(line) == m)
        parse(monthLines, MonthlyWorkReport(m, List()))
      )
end WorkHoursAnalyzer

// object Utils {
//   def openFile(path: String): List[String] = {
//     val source = scala.io.Source.fromFile(path)
//     val lines = source.getLines().toList
//     lines
//   }
// }

// @main def main(): Unit =
//   runTests()
//   val lines = Utils.openFile("inputZ_01.01.2026.txt")
//   val report = WorkHoursAnalyzer.MonthlyWorkReport.parse(lines)
//   val csvReport = report.generateCSV()
//   println(csvReport)
//   println(f"Total work hours in month ${report.month}: ${report.totalWorkTime.hour} hours and ${report.totalWorkTime.minute} minutes, in ${report.shifts.length} shifts, for a total additional time of ${report.totalAdditionalTime.hour} hours and ${report.totalAdditionalTime.minute} minutes (${report.totalAdditionalTime.toDoubleHours}).")



def runTests(): Unit =
  Tester.testTotalDurationPerShift()
  Tester.testMonthlyWorkReportParsing()
  println("All tests passed.")


object Tester:
  import WorkHoursAnalyzer._

  val exampleDayShiftLines = List(
    "IN 17 déc. 2025 à 07:30",
    "OUT 17 déc. 2025 à 12:00",
    "IN 17 déc. 2025 à 12:45",
    "OUT 17 déc. 2025 à 19:45"
  )
  val exampleNightShiftLines = List(
    "IN 17 déc. 2025 à 19:30",
    "OUT 18 déc. 2025 à 07:30"
  )

  val exampleMonthlyReportLines = List(
    "IN 17 déc. 2025 à 07:30",
    "OUT 17 déc. 2025 à 12:00",
    "IN 17 déc. 2025 à 12:45",
    "OUT 17 déc. 2025 à 19:45",
    "IN 18 déc. 2025 à 19:30",
    "OUT 19 déc. 2025 à 07:30",
    "IN 21 déc. 2025 à 07:30",
    "OUT 21 déc. 2025 à 19:45",
    "IN 23 déc. 2025 à 19:00",
    "OUT 24 déc. 2025 à 08:00",
    "IN 25 déc. 2025 à 07:23",
    "OUT 25 déc. 2025 à 12:00",
    "IN 25 déc. 2025 à 12:30",
    "OUT 25 déc. 2025 à 19:30",
    "IN 26 déc. 2025 à 07:30",
    "OUT 27 déc. 2025 à 00:30"
  )

  val example2MonthlyReportLines = List(
    "IN 17 janv. 2025 à 07:30",
    "OUT 17 janv. 2025 à 12:00",
    "IN 17 janv. 2025 à 12:45",
    "OUT 17 janv. 2025 à 19:45",
    "IN 18 janv. 2025 à 19:30",
    "OUT 19 janv. 2025 à 07:30",
    "IN 21 janv. 2025 à 07:30",
    "OUT 21 janv. 2025 à 19:45",
  )

  def testTotalDurationPerShift(): Unit =
    val dayShiftOpt = Shift.parse(exampleDayShiftLines)

    assert(dayShiftOpt.isDefined)
    val dayShift = dayShiftOpt.get
    assert(dayShift.totalWorkDuration.hour == 11 && dayShift.totalWorkDuration.minute == 30)

    val nightShiftOpt = Shift.parse(exampleNightShiftLines)
    assert(nightShiftOpt.isDefined)
    val nightShift = nightShiftOpt.get
    assert(nightShift.totalWorkDuration.hour == 12 && nightShift.totalWorkDuration.minute == 0)

  def testMonthlyWorkReportParsing(): Unit =
    val reports = MonthlyWorkReport.parse(exampleMonthlyReportLines)
    assert(reports.length == 1)
    val report = reports.head
    assert(report.month == 12)
    assert(report.shifts.length == 6)
    assert(report.shifts.head.dayOfMonth == 17)
    assert(report.shifts.head.typ == ShiftType.Day)

    assert(report.shifts(1).dayOfMonth == 18)
    assert(report.shifts(1).typ == ShiftType.Night)

    assert(report.shifts(2).dayOfMonth == 21)
    assert(report.shifts(2).typ == ShiftType.Day)
    assert(report.shifts(2).breakStartTime.isEmpty)

    assert(report.shifts(3).dayOfMonth == 23)
    assert(report.shifts(3).typ == ShiftType.Night)

    assert(report.shifts(4).dayOfMonth == 25)
    assert(report.shifts(4).typ == ShiftType.Day)

    assert(report.shifts(5).dayOfMonth == 26)
    assert(report.shifts(5).typ == ShiftType.Day)

    assert(report.shifts(5).breakStartTime.isEmpty)

    assert(report.totalWorkTime.hour == 77 && report.totalWorkTime.minute == 22)

end Tester