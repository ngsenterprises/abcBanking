package com.abc.common

package object CommnonUtil {
  val ONE_DAY = 24 * 3600 * 1000
}
import com.abc.common.CommnonUtil._

import java.util.{Calendar, Date}

object DateProvider {
  //use of lazy val changed by: ngs 10/21/16
  private[DateProvider] lazy val instance = new DateProvider
  def getInstance: DateProvider = instance
}

class DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }
}

