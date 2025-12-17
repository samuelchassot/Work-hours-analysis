object WorkHoursAnalyzer:

  case class Date(day: Int):
    require(1 <= day && day <= 32, "Day must be between 1 and 32") // 32 is used when a night shift spans two months
  case class Time(hour: Int, minute: Int):
    require(0 <= hour && hour < 24, "Hour must be between 0 and 23")
    require(0 <= minute && minute < 60, "Minute must be between 0 and 59")
  case class DateTime(date: Date, time: Time):
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

  object DateTime:
    def timeDifferenceMinutes(from: DateTime, to: DateTime): Int = 
      require(from < to, "The 'from' DateTime must happen before the 'to' DateTime")
      val minutesSinceStartOfMonthFrom = from.date.day * 24 * 60 + from.time.hour * 60 + from.time.minute
      val minutesSinceStartOfMonthTo = to.date.day * 24 * 60 + to.time.hour * 60 + to.time.minute
      minutesSinceStartOfMonthTo - minutesSinceStartOfMonthFrom


  enum ShiftType:
    case Day
    case Night
  
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
    require(totalWorkDuration._1 * 60 + totalWorkDuration._2 <= 24 * 60, "Total work duration must not exceed 24 hours")

    // We assume that shifts do not span more than 24 hours
    // We define that the day of a shift is determined by its start time
    def dayOfMonth: Int = startTime.getDayOfMonth

    /**
      * Return the total work duration of the shift, excluding breaks
      * in hours and minutes.
      *
      * @return
      */
    def totalWorkDuration: (Int, Int) =
      val totalDuration = DateTime.timeDifferenceMinutes(startTime, endTime)
      val breakDuration = (breakStartTime, breakEndTime) match
        case (Some(bs), Some(be)) => DateTime.timeDifferenceMinutes(bs, be)
        case _ => 0
      val workDuration = totalDuration - breakDuration
      val hours = workDuration / 60
      val minutes = workDuration % 60
      (hours, minutes)

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

          assert(if typ == ShiftType.Night then endTime.getDayOfMonth == startTime.getDayOfMonth + 1 else true)
          Some(Shift(typ, startTime, None, None, endTime))
        case _ => None

  case class MonthlyWorkReport(shifts: List[Shift]):
    /**
      * Calculate the total work hours and minutes for the month.
      *
      * @return
      */
    def totalWorkHours: (Int, Int) =
      val totalMinutes = shifts.map { shift =>
        val (hours, minutes) = shift.totalWorkDuration
        hours * 60 + minutes
      }.sum
      val totalHours = totalMinutes / 60
      val remainingMinutes = totalMinutes % 60
      (totalHours, remainingMinutes)

    /**
     * Generate a CSV report of the shifts.
      * 
      * Each line contains:
      * Day of month, Shift type (Day/Night), Start time, Break start time, Break end time, End time, Total work duration
      * 
      *
      * @return
      */
    def generateCSV(): String = 
      val header = "Day,Shift Type,Start Time,Break Start Time,Break End Time,End Time,Total Work Duration\n"
      val lines = shifts.map { shift =>
        val (workHours, workMinutes) = shift.totalWorkDuration
        val breakStartStr = shift.breakStartTime.map(dt => f"${dt.getHour}%02d:${dt.getMinute}%02d").getOrElse("")
        val breakEndStr = shift.breakEndTime.map(dt => f"${dt.getHour}%02d:${dt.getMinute}%02d").getOrElse("")
        s"${shift.dayOfMonth},${shift.typ},${f"${shift.startTime.getHour}%02d:${shift.startTime.getMinute}%02d"},$breakStartStr,$breakEndStr,${f"${shift.endTime.getHour}%02d:${shift.endTime.getMinute}%02d"},${f"$workHours%02d:$workMinutes%02d"}"
      }
      header + lines.mkString("\n")
  end MonthlyWorkReport

end WorkHoursAnalyzer

@main def runTests(): Unit =
  Tester.testTotalDurationPerShift()
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

  val 
  def testTotalDurationPerShift(): Unit =
    val dayShiftOpt = Shift.parse(exampleDayShiftLines)

    assert(dayShiftOpt.isDefined)
    val dayShift = dayShiftOpt.get
    val (workHours, workMinutes) = dayShift.totalWorkDuration
    assert(workHours == 11 && workMinutes == 30)

    val nightShiftOpt = Shift.parse(exampleNightShiftLines)
    assert(nightShiftOpt.isDefined)
    val nightShift = nightShiftOpt.get
    val (nightWorkHours, nightWorkMinutes) = nightShift.totalWorkDuration
    assert(nightWorkHours == 12 && nightWorkMinutes == 0)



end Tester