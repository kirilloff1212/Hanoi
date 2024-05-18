import scalafx.Includes.*
import scalafx.animation.AnimationTimer
import scalafx.animation.KeyFrame
import scalafx.animation.PathTransition
import scalafx.animation.Timeline
import scalafx.application.JFXApp3
import scalafx.application.Platform
import scalafx.delegate.*
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.*
import scalafx.scene.shape.Polyline
import scalafx.scene.text.Text
import scalafx.util.*

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.boundary.Label

object HanoiApp extends JFXApp3 {

  val diskHeight = 30 // Толщина одного диска башни
  val animspeed = 100d   // Скорость анимации
  val dist_pin = 350d     // Расстояние между башнями
  val first_pin_x = 250d  // Координата 1й башни
  val top_pin_y = 200d    // Высота подъема дисков
  val down_pin_y = 535d   // Нижняя точка дисков
  var iterationCount = 0  // Счетчик ходов

  implicit val ec: ExecutionContext = ExecutionContext.global

  // Функция создания платформы башни
  def platform(xr: Double, yr: Double) = new Rectangle {
    x = xr
    y = yr
    width = 250
    height = 25
    arcHeight = 10
    arcWidth = 10
    fill = web("#421900")
  }
  // Функция создания штыря башни
  def pin(xr: Double, yr: Double) = new Rectangle {
    x = xr
    y = yr
    width = 16
    height = 300
    arcHeight = 16
    arcWidth = 16
    fill = web("#421900")
  }
  // Функция создания диска башни
  def disk(xr: Double, yr: Double, wr: Double, color: Color) = new Rectangle {
    x = xr
    y = yr
    width = wr
    height = diskHeight
    arcHeight = 30
    arcWidth = 30

    fill = color
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Ханойские башни"
      width = 1200                                  // Размеры окна
      height = 800
      scene = new Scene {

        fill = Gray                                 // Заливка фона

        val platform_1 = platform(135, 550)         // Создание платформ башен
        val platform_2 = platform(485, 550)         // (Координата x, Координата y)
        val platform_3 = platform(835, 550)

        val pin_1 = pin(252, 265)                   // Создание штырей башен
        val pin_2 = pin(602, 265)                   // (Координата x, Координата y)
        val pin_3 = pin(952, 265)

        val disk_1 = disk(160, 520, 200, Indigo)    // Создание дисков ханойской башни
        val disk_2 = disk(170, 490, 180, Violet)    // (Координата x, Координата y, Ширина, Цвет)
        val disk_3 = disk(180, 460, 160, Blue)
        val disk_4 = disk(190, 430, 140, LightBlue)
        val disk_5 = disk(200, 400, 120, Green)
        val disk_6 = disk(210, 370, 100, Yellow)
        val disk_7 = disk(220, 340, 80, Orange)
        val disk_8 = disk(230, 310, 60, Red)

        val iterationText = new Text(575, 100, s"Ход номер: $iterationCount") //Создание счетчика ходов

        // Инициализация башен
        var buf1 = Seq[(Int, Rectangle)]((1, disk_1), (2, disk_2), (3, disk_3), (4, disk_4), (5, disk_5), (6, disk_6), (7, disk_7), (8, disk_8))
        var buf2 = Seq[(Int, Rectangle)]()
        var buf3 = Seq[(Int, Rectangle)]()

        // Рекурсивная функция решения задачи
        def solve(n: Int, from: Int, to: Int, via: Int): Future[Unit] = {
          if (n == 1) {
            move(from, to, getTopDisk(from).getOrElse((0, new Rectangle))._2)
              .map(_ => {
                moveDisk(from, to)
                iterationCount += 1 // Увеличиваем счетчик после перемещения диска
                Platform.runLater {
                  iterationText.text = s"Ход номер: $iterationCount"
                }
              })
          } else {
            for {
              _ <- solve(n - 1, from, via, to)
              _ <- move(from, to, getTopDisk(from).getOrElse((0, new Rectangle))._2).map(_ => {
                moveDisk(from, to)
                iterationCount += 1
                Platform.runLater {
                  iterationText.text = s"Ход номер: $iterationCount"
                }
              })
              _ <- solve(n - 1, via, to, from)
            } yield ()
          }
        }

        def moveDisk(from: Int, to: Int): Unit = {
          getTopDisk(from) match {
            case Some(disk) =>
              // Удаляем диск из башни источника
              if (from == 1) {
                buf1 = buf1.dropRight(1)
              } else if (from == 2) {
                buf2 = buf2.dropRight(1)
              } else if (from == 3) {
                buf3 = buf3.dropRight(1)
              }
              // Добавляем диск в башню назначения
              if (to == 1) {
                buf1 = buf1 :+ disk
              } else if (to == 2) {
                buf2 = buf2 :+ disk
              } else if (to == 3) {
                buf3 = buf3 :+ disk
              }
            case None => // Случай пустой башни
          }
        }
        // Фунция получения верхнего диска
        def getTopDisk(pole: Int): Option[(Int, Rectangle)] = {
          if (pole == 1 && buf1.nonEmpty) {
            Some(buf1.last)
          } else if (pole == 2 && buf2.nonEmpty) {
            Some(buf2.last)
          } else if (pole == 3 && buf3.nonEmpty) {
            Some(buf3.last)
          } else {
            None
          }
        }
        // Функция определения высоты башни
        def getDiskStackHeight(pole: Int): Double = {
          if (pole == 1) {
            buf1.length * diskHeight
          } else if (pole == 2) {
            buf2.length * diskHeight
          } else if (pole == 3) {
            buf3.length * diskHeight
          } else {
            0d
          }
        }
        // Функция анимации перемещения
        def move(sourc: Int, dest: Int, disk: Rectangle): Future[Unit] = {
          val sourceY = down_pin_y - getDiskStackHeight(sourc) + diskHeight
          val sourceX = (sourc - 1) * dist_pin + first_pin_x
          val destX = (dest - 1) * dist_pin + first_pin_x
          val destY = 550 - diskHeight/2 - getDiskStackHeight(dest)
          val line = new Polyline(javafx.scene.shape.Polyline(sourceX + 10, sourceY, sourceX + 10, top_pin_y,
            destX + 10, top_pin_y, destX + 10, destY))
          val transit = new PathTransition()
          transit.setDuration(javafx.util.Duration(animspeed))
          transit.setPath(line)
          transit.setNode(disk)
          transit.setOrientation(PathTransition.OrientationType.None)
          transit.setAutoReverse(false)
          transit.setCycleCount(1)
          val promise = Promise[Unit]()
          transit.setOnFinished(event => promise.success(()))
          transit.play()
          promise.future
        }
        // Анимация программы
        var iteration = 1
          val animation = AnimationTimer((now) => {
            if (iteration == 1) {
              Platform.runLater {
                content = Seq(platform_1, platform_2, platform_3, pin_1, pin_2, pin_3, disk_1, disk_2, disk_3, disk_4, disk_5, disk_6, disk_7, disk_8, iterationText)
                solve(8, 1, 3, 2)
              }
              iteration = 0
            }
          })
          animation.start()
      }
    }
  }
}