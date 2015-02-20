import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat

trait Colors {
	val LabelColorNeutre = new Color(255,100,0)
	val LabelColorActif = new Color(255,200,100)
	val LabelColorDetected = new Color(255,50,50)
	val ColorNum = List(
		new Color(255,255,255),
		new Color(0,0,255),
		new Color(0,200,0),
		new Color(255,0,0),
		new Color(0,255,255),
		new Color(150,0,175),
		new Color(50,200,120),
		new Color(200,120,50),
		new Color(0,200,200)
	)
	val ColorBomb = new Color(0,0,0)
}

trait CaseLabelAttribute {
	val CaseSize = 40
	val Marge = 15
	val LabelBordure = Swing.LineBorder(new Color(0,0,0,100))
}

class CaseLabel (t : UI, n : Int) extends Label with Colors with CaseLabelAttribute {
	font = new Font("Arial", 1, 32)
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
		if (flag()) {
			background = LabelColorNeutre
			t.majBomb(-1)
		}
		else {
			background = LabelColorDetected
			t.majBomb(1)
		}
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

class NumberField(x : String) extends TextField(x) {
	listenTo(keys)
	reactions += {
		case e : KeyTyped =>
			if (!e.char.isDigit)
				e.consume
	}
}

class Form(t : String, s : IndexedSeq[Label], d : IndexedSeq[String]) extends Dialog {
	title = t
	var accepted = false
	modal = true
	var lst = d map (x => new NumberField(x))
	def f = {
		accepted = true
		visible = false
	}
	contents = new GridPanel(s.length + 1, 2) {
		for (i <- 0 until s.length) {
			contents += s(i)
			contents += lst(i)
		}
		contents += new Label("")
		contents += new Button("") {
			action = Action("Jouer")(f)
		}
	}
	visible = true
}

class GrilleMode(t : UI, m : Int, x : Int = 0, y : Int = 0, b : Int = 0) extends MenuItem("") with CaseLabelAttribute {
	private var x_grille = x
	private var y_grille = y
	private var b_grille = b
	def restart = {
		t.lstLabel.foreach(l => l.clear())
		t.nbDiscovered = 0
		t.nbDetected = 0
		t.majBomb(0)
		t.timeBegin = new Date()
		t.inGame = true
		t.labelFin.text = ""
	}
	def custom = {
		var window = new Form(
			"Grille Perso",
			IndexedSeq(new Label("x : "), new Label("y : "),  new Label("b : ")),
			IndexedSeq("6", "7", "10")
		)
		x_grille = window.lst(0).text.toInt
		y_grille = window.lst(1).text.toInt
		b_grille = window.lst(2).text.toInt
		if (window.accepted && x_grille > 0 && x_grille < 16 && y_grille > 0 && y_grille < 16 && x_grille * y_grille > 9 && b_grille + 9 <= x_grille * y_grille)
			creat
	}
	def creat = {
		var grid = new GridPanel(y_grille,x_grille) {
                	t.lstLabel = 0 until (x_grille * y_grille) map (n => new CaseLabel(t, n){
                        	border = Swing.LineBorder(new Color(0,0,0,255),1)
                        })
                        contents ++= t.lstLabel
                }
		var aff = new FlowPanel() {
                	contents += t.labelBomb
                	contents += t.labelFin
			contents += t.labelChrono
			t.labelBomb.preferredSize = new Dimension(x_grille * CaseSize / 3, 30)
			t.labelFin.preferredSize = new Dimension(x_grille * CaseSize / 3, 30)
			t.labelChrono.preferredSize = new Dimension(x_grille * CaseSize / 3, 30)
                }
		t.contents = new BorderPanel {
			layout(grid) = North
			layout(aff) = South
		}
		t.x = x_grille
	        t.y = y_grille
        	t.nbDiscovered = 0
        	t.nbBombs = b_grille
		t.nbDetected = 0
		t.labelBomb.text = ""
		t.majBomb(0)
		t.timeBegin = new Date
		t.inGame = true
		var df = new SimpleDateFormat("mm:ss")
		var timer = new javax.swing.Timer(1000, Swing.ActionListener(e => {
			var minutes  = ((new Date).getTime() - t.timeBegin.getTime()) / 60000 % 60
			var secondes = ((new Date).getTime() - t.timeBegin.getTime()) / 1000 % 60
			var str = if (minutes < 10) "0" else ""
			str = str + minutes.toString + ":"
			str = if (secondes < 10) str + "0" else str
			str = str + secondes.toString
			if (t.inGame == true)
                        	t.labelChrono.text = str
    		}))
    		timer.start()
	}
	if (m == 0)
		action = Action("Grille " + x_grille + "x" + y_grille + " (" + b_grille + ")")(creat)
	else if (m == 1) {
                action = Action("Restart")(restart)
        }
	else if (m == 2) {
		action = Action("Grille Personnalisée")(custom)
	}
}


class UI extends MainFrame with Colors {
	var x = 5
	var y = 3
	var t = this
	var nbDiscovered = 0
	var nbDetected = 0
	var nbBombs = 0
	var inGame = false
	var timeBegin = new Date()
	var lstLabel : IndexedSeq[CaseLabel] = IndexedSeq()
	var labelBomb = new Label()
	var labelFin = new Label()
	var labelChrono = new Label()
	resizable = false
	title = "Démineur"
	contents = new Label("Welcome ! ;)") {
		preferredSize = new Dimension(300,300)
	}
	menuBar = new MenuBar {
                contents += new Menu("Game") {
                        contents += new GrilleMode(t,0,9,9,10)
			contents += new GrilleMode(t,0,16,16,40)
			contents += new GrilleMode(t,0,16,16,99)
			contents += new GrilleMode(t,1)
			contents += new GrilleMode(t,2)
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
	def majBomb(n : Int) = {
		nbDetected = nbDetected + n
		labelBomb.text = "B : " + nbDetected.toString + " / " + nbBombs.toString
		if (nbDetected > nbBombs)
			labelBomb.foreground = LabelColorDetected
		else
			labelBomb.foreground = new Color(0,0,0)
	}
	def neighbour(n : Int) : List[Int] = {
		var lst : List[Int]= List()
		var a = if (n % x == 0) 0 else -1
                var b = if (n % x == x - 1) 0 else 1
                var c = if (n < x) 0 else -1
                var d = if (n >= (y - 1) * x) 0 else 1
		for (i <- a to b) {
                        for (j <- c to d) {
                                if (0 <= n + j * x + i && n + j * x + i < x * y) {
                                        lst ++= List(n + j * x + i)
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
		inGame = false
		labelFin.text = "GAME OVER !"
		labelFin.background = new Color(255,0,0)
		lstLabel.foreach(x => x.deafTo(x.mouse.moves, x.mouse.clicks))
	}
	def win() = {
		inGame = false
                labelFin.text = "WIN !"
                labelFin.background = new Color(0,255,0)
		lstLabel.foreach(x => x.deafTo(x.mouse.moves, x.mouse.clicks))
	}
}

object GuiProgramOne {
	def main(args: Array[String]) {
		val ui = new UI
		ui.visible = true
	}
}
