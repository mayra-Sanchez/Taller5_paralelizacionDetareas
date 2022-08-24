import common._

package object scalashop {
  /** El valor de cada pixel es representado con un entero de 32 bits. */
  type RGBA = Int

  /** Devuelve el componente de rojo */
  def rojo(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Devuelve el componente de verde */
  def verde(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Devuelve el componente de azul */
  def azul(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Devuelve el componente  alpha */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Usado para crear un valor RGBA a partir de sus componentes */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restringe el entero al rango especificado */
  def cercar(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** La imagen es un arreglo de dos dimensiones de valores de pixeles */
  class Img(val ancho: Int, val alto: Int, private val datos: Array[RGBA]) {
    def this(an: Int, al: Int) = this(an, al, new Array(an*al))
    def apply(x: Int, y: Int): RGBA = datos(y * ancho + x)
    def update(x: Int, y: Int, c: RGBA): Unit = datos(y * ancho + x) = c
  }

  /** Calcula el valor RGBA del pixel desenfocado correspondiente a un pixel de la imagen de entrada. */
  def desenfoqueNuclear(fte: Img, x: Int, y: Int, radio: Int): RGBA ={
    val minX = cercar(x - radio, 0, fte.ancho - 1)
    val maxX = cercar(x + radio, 0, fte.ancho - 1)
    val minY = cercar(y - radio, 0, fte.alto - 1)
    val maxY = cercar(y + radio, 0, fte.alto - 1)
    var accX = minX
    var accY = minY
    var pixelProcessed = 0
    var accR = 0
    var accG = 0
    var accB = 0
    var accA = 0
    while(accX <= maxX) {
      while(accY <= maxY) {
        val pixel = fte.apply(accX, accY)
        accR += rojo(pixel)
        accG += verde(pixel)
        accB += azul(pixel)
        accA += alpha(pixel)
        pixelProcessed += 1
        accY += 1
      }
      accX += 1
      accY = minY
    }
    rgba(accR / pixelProcessed, accG / pixelProcessed, accB / pixelProcessed, accA / pixelProcessed)
  }
    // POR HACER: puede implementarla usando ciclos while o expresiones for
  object ListZipper {

      def zipElements(numElements: Int, numTasks: Int) = {
        val range = 0 to numElements by numTasks
        range.zip(range.tail :+ numElements).toList
      }
  }

}
