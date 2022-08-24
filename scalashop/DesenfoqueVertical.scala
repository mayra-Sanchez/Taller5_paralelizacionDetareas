package scalashop

import common.task
import org.scalameter.{Key, Warmer, config}

object EjecutorDesenfoqueVertical {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit = {
    val radio = 6
    val ancho = 1920
    val alto = 1080
    val fte = new Img(ancho, alto)
    val dst = new Img(ancho, alto)
    val seqtime = standardConfig measure {
      DesenfoqueVertical.desenfoque(fte, dst, 0, ancho, radio)
    }
    println(s"tiempo de desenfoque secuencial: $seqtime ms")

    val numTareas = 32
    val partime = standardConfig measure {
      DesenfoqueVertical.desenfoquePar(fte, dst, numTareas, radio)
    }
    println(s"tiempo de desenfoque paralelo: $partime ms")
    println(s"aceleracion: ${seqtime.value / partime.value}")
  }

}

/** Una computación sencilla trivialmente paralelizable */
object DesenfoqueVertical {

  /** Desenfoca las columnas de la imagen fuente 'fte' en la imagen destino 'dst'
   * empezando desde la columna 'inicial' y terminando con la columna 'fin' (exclusive)
   *
   * Dentro de cada columna, el 'desenfoque' atraviesa los pixels de arriba a abajo
   */
  def desenfoque(fte: Img, dst: Img, inicial: Int, fin: Int, radio: Int): Unit = {
    for(
      row <- inicial until fin;
      column <- 0 until fte.alto;
      if row >= 0 && row < fte.ancho
    ) yield {
      dst.update(row, column, desenfoqueNuclear(fte, row, column, radio))
    }
    // POR HACER:  implemente esta  funcion usando el la funcion `desenfoqueNuclear`
  }

  /**  Desenfoca las columnas de la imagen fuente 'fte' en la imagen destino 'dst'
   *  en paralelo usando 'numTareas' tareas.
   *
   *  La paralelizacion se hace dividiendo la imagen fuente 'fte' en  conjuntos de columnas,
   *  cada conjunto correspondiente a una de las 'numTareas' en que se hace el calculo en paralelo
   *
   */
  def desenfoquePar(fte: Img, dst: Img, numTareas: Int, radio: Int): Unit = {
    // POR HACER:  implemente usando el constructor 'task' y la funcion 'desenfoque'
    //for (i <- 0 until fte.ancho; j <- 0 until fte.alto) dst.update(i,j, fte(i,j))
    val separateStrips = 0 to fte.ancho by (fte.ancho / numTareas max 1)

    separateStrips.zip(separateStrips.tail)
      .map { case (from, end) =>
        task[Unit] {
          desenfoque(fte, dst, from, end, radio)
        }
      }
      .foreach(_.join())
  }

}