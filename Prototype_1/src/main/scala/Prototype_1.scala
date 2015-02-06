import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon

class CaseLabel (t : UI, n : Int) extends Label {
	icon_= ( new ImageIcon("OrangeSquareIcon"))
	preferredSize = new Dimension(t.Case_Size,t.Case_Size)
	private var discovered = false
	var v = "?"
	var num = n
        listenTo(mouse.moves, mouse.clicks)
        reactions += {
                case e : MouseEntered =>
			if (!discovered)
                        	border = Swing.LineBorder(new Color(255,0,0))
                case e : MouseExited =>
			if (!discovered)
                		border = Swing.LineBorder(new Color(0,0,0))
		case e : MouseClicked =>
			discoverMe()
        }
	def discoverMe() : Unit = {
		if (!discovered) {
			border = Swing.LineBorder(new Color(0,0,255))
                	discovered = true
			t.add()
			if (v == "?")
				t.init(n)
			text = v
			v match {
				case "b" => t.lose()
				case "0" => t.spread(n)
				case _   => ()
			}
		}
	}
	def clear() = {
		text = ""
		discovered = false
		border = Swing.LineBorder(new Color(0,0,0))
	}
}

class GrilleMode(t : UI, x : Int = 0, y : Int = 0, b : Int = 0) extends MenuItem(""){
	def f = {
		t.contents = new GridPanel(y,x){
                	t.lstLabel = 0 until (x * y) map (n => new CaseLabel(t, n){
                        	border = Swing.LineBorder(new Color(0,0,0))
                        })
                	contents ++= t.lstLabel
        t.preferredSize = new Dimension(x*(t.Case_Size) +15,y*(t.Case_Size) +15)
		}
		t.x = x
	        t.y = y
        	t.nbDiscovered = 0
        	t.nbBombs = b
	}
	action = Action("Grille " + x + "x" + y + " (" + b + ")")(f)
	if (b == 0) {
                action = new Action("Restart") {
                        def apply {
                                println(text)
				t.lstLabel.foreach(l => l.clear())
				t.nbDiscovered = 0
			}
                }
        }
}


class UI extends MainFrame {
	val Case_Size = 50
	var x = 5
	var y = 3
	var t = this
	var nbDiscovered = 0
	var nbBombs = 0
	var lstLabel : IndexedSeq[CaseLabel] = IndexedSeq()
	title = "Démineur"
	preferredSize = new Dimension(300,300)
	contents = new Label("Welcome ! ;)")
	menuBar = new MenuBar {
                contents += new Menu("Game") {
                        contents += new GrilleMode(t, 9,9,10)
			contents += new GrilleMode(t, 16,16,40)
			contents += new GrilleMode(t, 16,16,99)
			contents += new GrilleMode(t)
                }
		contents += new Menu("About") {
                        contents += new MenuItem(new Action("Prout") {
                                def apply {
                                        println("About what ??")
                                }
                        })
		}
        }
	def add() = {
		nbDiscovered += 1
		if (nbDiscovered + nbBombs == x * y)
			win()
	}
	def neighbour(n : Int) : List[Int] = {
		var lst : List[Int]= List()
		var a = if (n % y == 0) 0 else -1
                var b = if (n % y == y - 1) 0 else 1
                var c = if (n < y) 0 else -1
                var d = if (n >= (x - 1) * y) 0 else 1
                for (i <- a to b) {
                        for (j <- c to d) {
                                if (0 <= n + j * x + i && n + j * x + i < x * y) {
                                        lst ++= List(n + j * x + i) // LOLILOOOL
                                }
                        }
                }
		return lst	
	}
	def spread(n : Int) = {
		var lst = neighbour(n)
		lst.foreach(n => lstLabel(n).discoverMe())
	}
	def init(k : Int) = {
		var b = nbBombs
		var r = scala.util.Random
		neighbour(k).foreach(n => lstLabel(n).v = "#")
		while (b > 0) {
			var j = r.nextInt(x * y)
			if (lstLabel(j).v == "?") {
				lstLabel(j).v = "b"
				b -= 1
			}
		}
		lstLabel.foreach(l =>
			if (l.v != "b") {
				var v = 0
				neighbour(l.num).foreach(n => if (lstLabel(n).v == "b") v += 1)
				l.v = v.toString
			}
		)
	}
	def lose() = {
		println("lose")
	}
	def win() = {
		println("win")
	}
}

object GuiProgramOne {
	def main(args: Array[String]) {
		val ui = new UI
		ui.visible = true
	}
}
