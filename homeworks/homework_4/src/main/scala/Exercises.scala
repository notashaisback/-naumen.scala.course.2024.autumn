import scala.annotation.tailrec
import scala.util.Random

object Exercises {

  /**
   * Задание №1
   * Дана императивная функция findSumImperative.
   * Напишите ее аналог (findSumFunctional) в функциональном стиле.
   *
   * ПОДСКАЗКА
   * Стоит воспользоваться методами, которые предоставляет объект List или рекурсией.
   * Страница с полезностями List: https://alvinalexander.com/scala/list-class-methods-examples-syntax/
   */
  def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
    var result: (Int, Int) = (-1, -1)
    for (i <- 0 until items.length) {
      for (j <- 0 until items.length) {
        if (items(i) + items(j) == sumValue && i != j) {
          result = (i, j)
        }
      }
    }
    result
  }

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    val pairs = for {
      i <- items.indices
      j <- items.indices
      if i != j && items(i) + items(j) == sumValue
    } yield (i, j)

    if (pairs.isEmpty) (-1, -1) else pairs.last
  }


  /**
   * Задание №2
   *
   * Дана рекурсивная функция simpleRecursion.
   * Перепишите ее так, чтобы получилась хвостовая рекурсивная функция.
   *
   * Для прохождения теста на большое количество элементов в списке
   * используйте анотацию @tailrec к вашей функции.
   */
  def simpleRecursion(items: List[Int], index: Int = 1): Int = {
    items match {
      case head :: tail =>
        if (head % 2 == 0) {
          head * simpleRecursion(tail, index + 1) + index
        } else {
          -1 * head * simpleRecursion(tail, index + 1) + index
        }
      case _ => 1
    }
  }

  def tailRecRecursion(items: List[Int]): Int = {
    @tailrec
    def loop(remaining: List[Int], index: Int, acc: Int): Int = {
      remaining match {
        case head :: tail =>
          val newAcc = if (head % 2 == 0) {
            head * acc + index
          } else {
            -head * acc + index
          }
          loop(tail, index - 1, newAcc)
        case Nil => acc
      }
    }

    if (items.isEmpty) 1
    else loop(items.reverse, items.length, 1)
  }

  /**
   * Задание №3
   * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def search(left: Int, right: Int): Option[Int] = {
      if (left > right) None
      else {
        val mid = left + (right - left) / 2
        items(mid) compare value match {
          case 0 => Some(mid)
          case -1 => search(mid + 1, right)
          case 1 => search(left, mid - 1)
        }
      }
    }

    if (items.isEmpty) None
    else search(0, items.length - 1)
  }

  /**
   * Задание №4
   * Реализуйте функцию, которая генерирует список заданной длинны c именами.
   * Функция должна соответствовать всем правилам функционального программирования.
   *
   * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
   */

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount <= 0) return List.empty

    val vowels = "aeiou"
    val consonants = "bcdfghjklmnpqrstvwxyz"
    val random = new Random()

    def generateName: String = {
      val nameLength = random.nextInt(6) + 3 // 3-8 characters
      val firstChar = consonants.charAt(random.nextInt(consonants.length)).toUpper

      val restChars = (1 until nameLength).map { i =>
        if (i % 2 == 1) vowels.charAt(random.nextInt(vowels.length))
        else consonants.charAt(random.nextInt(consonants.length))
      }.mkString

      firstChar + restChars
    }

    @tailrec
    def generateUniqueNames(count: Int, acc: Set[String]): Set[String] = {
      if (count <= 0) acc
      else {
        val newName = generateName
        if (acc.contains(newName)) generateUniqueNames(count, acc)
        else generateUniqueNames(count - 1, acc + newName)
      }
    }

    generateUniqueNames(namesCount, Set.empty).toList
  }
}

/**
 * Задание №5
 *
 * Дана реализация сервиса по смене номера SimpleChangePhoneService с методом changePhone
 * Необходимо написать реализацию этого сервиса с учетом правил работы со сторонними эффектами (SideEffects).
 *
 * Для этого необходимо сначала реализовать собственный сервис работы с телефонными номерами (PhoneServiceSafety),
 * используя при этом методы из unsafePhoneService.
 * Методы должны быть безопасными, поэтому тип возвращаемых значений необходимо определить самостоятельно.
 * Рекомендуется воспользоваться стандартными типами Scala (например Option или Either).
 *
 * Затем, с использованием нового сервиса, необходимо реализовать "безопасную" версию функции changePhone.
 * Функция должна возвращать ok в случае успешного завершения или текст ошибки.
 *
 * Изменять методы внутри SimplePhoneService не разрешается.
 */

object SideEffectExercise {
  trait ChangePhoneService {
    def changePhone(oldPhone: String, newPhone: String): String
  }

  class SimplePhoneService {
    def findPhoneNumber(phone: String): String = ???
    def deletePhone(phone: String): String = ???
    def addPhoneToBase(phone: String): String = ???
  }

  class SimpleChangePhoneService(phoneService: SimplePhoneService) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneRecord = phoneService.findPhoneNumber(oldPhone)
      if (oldPhoneRecord != null) {
        phoneService.deletePhone(oldPhoneRecord)
      }
      phoneService.addPhoneToBase(newPhone)
      "ok"
    }
  }

  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {
    def findPhoneNumberSafe(phone: String): Option[String] = {
      Option(unsafePhoneService.findPhoneNumber(phone))
    }

    def deletePhoneSafe(phone: String): Either[String, Unit] = {
      val result = unsafePhoneService.deletePhone(phone)
      if (result != null) Left(result) else Right(())
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      val result = unsafePhoneService.addPhoneToBase(phone)
      if (result != null) Left(result) else Right(())
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val result = for {
        _ <- phoneServiceSafety.findPhoneNumberSafe(oldPhone)
          .map(phoneServiceSafety.deletePhoneSafe)
          .getOrElse(Right(()))
        _ <- phoneServiceSafety.addPhoneToBaseSafe(newPhone)
      } yield ()

      result.fold(identity, _ => "ok")
    }
  }
}
