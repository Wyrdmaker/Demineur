import scala.swing._
import scala.swing.event._
import javax.swing.{ImageIcon, Icon}

trait Colors {
	val LabelColorNeutre = new Color(255,100,0)
	val LabelColorActif = new Color(255,200,100)
	val LabelColorDetected = new Color(255,50,50)
	val ColorNum = List(
		new Color(255,255,255),
		new Color(0,0,255),
		new Color(0,255,0),
		new Color(255,0,0),
		new Color(100,100,100),
		new Color(150,0,175),
		new Color(50,200,120),
		new Color(200,120,50),
		new Color(0,200,200)
	)
	val ColorBomb = new Color(0,0,0)
}

trait CaseLabelAttribute {
	val CaseSize = 50
	val Marge = 15
	val LabelBordure = Swing.LineBorder(new Color(0,0,0,100))
}

class CaseLabel (t : UI, n : Int) extends Label with Colors with CaseLabelAttribute {
	repaint()
	preferredSize = new Dimension(CaseSize,CaseSize)
	private var discovered = false
	var v = "?"
	var num = n
	clear()
        reactions += {
                case e : MouseEntered =>
			if (!discovered) 
                    		border = Swing.LineBorder(new Color(0,0,255),1)
                case e : MouseExited =>
			if (!discovered)
                		border = Swing.LineBorder(new Color(0,0,0),1)
		case e : MouseClicked =>
			if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1 && !flag())
				discoverMe()
			else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3)
				switch()
	}
	def flag(): Boolean = {
		return background == LabelColorDetected
	}

	def switch() : Unit = {
		background = if (flag()) LabelColorActif else LabelColorDetected
	}
	def discoverMe() : Unit = {
		if (!discovered) {
			deafTo(mouse.moves, mouse.clicks)
			repaint()
			background = LabelColorActif
			border = LabelBordure
                	discovered = true
			t.add()
			if (v == "?")
				t.init(n)
			text = v
			v match {
				case "b" =>
					background = LabelColorDetected
					foreground = ColorBomb
					t.lose()
				case "0" =>
					foreground = ColorNum(text.toInt)
					t.spread(n)
				case _   =>
					foreground = ColorNum(text.toInt)
			}
		}
	}
	def clear() : Unit = {
		opaque = true
		text = ""
		discovered = false
		listenTo(mouse.moves, mouse.clicks)
		background = LabelColorNeutre
		border = Swing.LineBorder(new Color(0,0,0))
	}
}

class GrilleMode(t : UI, x : Int = 0, y : Int = 0, b : Int = 0) extends MenuItem("") with CaseLabelAttribute {
	def creat = {
		t.contents = new GridPanel(y + 1,x){
                	t.lstLabel = 0 until (x * y) map (n => new CaseLabel(t, n){
                        	border = Swing.LineBorder(new Color(0,0,0,255),1)
                        })
                	contents ++= t.lstLabel
			contents += new FlowPanel(new Label("B : "), new Label("0/10"), new Label(""))
        		t.preferredSize = new Dimension(x * CaseSize + Marge,y * CaseSize + Marge)
		}
		t.x = x
	        t.y = y
        	t.nbDiscovered = 0
        	t.nbBombs = b
	}
	action = Action("Grille " + x + "x" + y + " (" + b + ")")(creat)
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
			contents += new GrilleMode(t,16,16,99)
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
		lstLabel.foreach(x => x.deafTo(x.mouse.moves, x.mouse.clicks))
	}
	def win() = {
		println("win")
		lstLabel.foreach(x => x.deafTo(x.mouse.moves, x.mouse.clicks))
	}
}

object GuiProgramOne {
	def main(args: Array[String]) {
		val ui = new UI
		ui.visible = true
	}
}
