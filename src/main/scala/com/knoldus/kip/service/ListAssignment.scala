package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Marks, Student}

trait ListAssignment {

  //Assignment -1
  def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {

    passOrFail match {
      case "pass" => RamDatabase.marksList.count(a => a.subjectId == subjectId && a.marksObtained >= percentage)
      case "fail" => RamDatabase.marksList.count(a => a.subjectId == subjectId && a.marksObtained < percentage)
    }
  }

  def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {

    val sortedMarks: List[Marks] = RamDatabase.marksList.filter(_.subjectId == subjectId).sortWith(_.marksObtained >= _.marksObtained)

    val students: List[Student] = sortedMarks.flatMap(x => RamDatabase.studentList.filter(_.id == x.studentId))

    topOrBottom match {
      case "top" => students.take(count)
      case "bottom" => students.takeRight(count)
    }
  }

  def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {

    val total: List[Float] = RamDatabase.studentList.map(x => RamDatabase.marksList.filter(_.studentId == x.id).map(_.marksObtained).sum)

    val s = RamDatabase.studentList.zip(total).sortBy(_._2).reverse

    topOrBottom match {
      case "top" => s.take(count).map(_._1)
      case "bottom" => s.takeRight(count).map(_._1)

    }
  }

  def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int)
  : (List[(Student, Int)], List[(Student, Int)]) = {

    val noOfSubjects = RamDatabase.marksList.length / RamDatabase.studentList.length

    val percent: List[Int] = RamDatabase.studentList.map(x => RamDatabase.marksList.filter(_.studentId == x.id).map(_.marksObtained).sum.toInt / noOfSubjects)

    val zippedList = RamDatabase.studentList.zip(percent)

    val p: (List[(Student,Int)], List[(Student,Int)]) = zippedList.partition(_._2 >= percentage)

    (p._1.map(x => (x._1, goodScholarship)) , p._2.map(x => (x._1, normalScholarship)))

  }

  def studentsWithMoreThan95: List[Student] = passedOrFailed("pass", 95)

  def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {


    val totalOfEachStudent: List[Float] = RamDatabase.studentList.map(x => RamDatabase.marksList.filter(_.studentId == x.id).map(_.marksObtained).sum)

    val maxMarks = RamDatabase.marksList.count(_.studentId == RamDatabase.marksList(0).studentId) * 100

    val percentOfEach = totalOfEachStudent.map(_ / maxMarks * 100)

    val zippedList = RamDatabase.studentList.zip(percentOfEach)


    passOrFail match {

      case "pass" => zippedList.filter(_._2 >= percentage).map(_._1)
      case "fail" => zippedList.filter(_._2 < percentage).map(_._1)
    }

  }

  def generateReport: List[(String, List[Int])] = {

    val marks: List[List[Int]] = RamDatabase.studentList.map(x => RamDatabase.marksList.groupBy(_.studentId == x.id)(true)
      .map(_.marksObtained.toInt))

    val student_name = RamDatabase.studentList.map(_.name)

    student_name.zip(marks)

  }

  //Assignment - 2
  def getLastElementWithIndex(list: List[String]): (String, Int) = {

    def compute(li: List[String], n: Int): (String, Int) = li match {
      case head :: Nil => (head, n)
      case head :: tail => compute(tail, n + 1)
      case _ => ("Empty list", -1)
    }
    compute(list, 0)
  }


  def printTable(list: List[Long]): List[Long] ={val x = 1 to 10 toList

    list.flatMap(y => x.map(_*y))
  }


  def aggregateLists(list1: List[String], list2: List[Long]): List[List[(String, Long)]] = list1.zip(list2).map(x => List(x))


  def getSumOfList(list: List[Long]): Long = list match {
    case Nil => 0
    case head :: tail => head + getSumOfList(tail)
  }

  def getMultiplicationOfList(list: List[Long]): Long = list match {
    case Nil => 1
    case head :: tail => head * getMultiplicationOfList(tail)
  }

  def quickSortList(list: List[Long]): List[Long] = list match {
    case Nil => Nil
    case head :: tail => quickSortList(tail.filter(_ < head)) ::: head :: quickSortList(tail.filter(_ >= head))
  }


  def mergeSortList(list: List[Long]): List[Long] = {
    val n = list.length / 2
    if (n == 0) {
      list
    }
    else {
      val (left, right) = list.splitAt(n)
      merge(mergeSortList(left), mergeSortList(right))
    }
  }

  def merge(left: List[Long], right: List[Long]): List[Long] = (left, right) match {
    case (left, Nil) => left
    case (Nil, right) => right
    case (leftHead :: leftTail, rightHead :: rightTail) => if (leftHead < rightHead) {
      leftHead :: merge(leftTail, right)
    }
    else rightHead :: merge(left, rightTail)
  }

}
