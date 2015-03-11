import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat

abstract class Demineur_Label_State extends Label_State[Demineur_Label] {
	//val size_x = Demineur.square_size_x
	//val size_y = Demineur.square_size_y
	val opaque = true
	val foreground = GUI_GE.black
}

class Label_State_Unexplored extends Demineur_Label_State{
	val state_name = "unexplored"

	val label_border = DGE.border(DGE.black)
	val background = DGE.label_color_unexplored
	val text = ""
}

class Label_State_Explored extends Demineur_Label_State {
	val state_name = "explored"

	val label_border = DGE.border(DGE.black_dim)
	val background = DGE.label_color_explored
	val text = ""
	override def change_to_state(d_label: Demineur_Label) = {
		super.change_to_state(d_label)
		d_label.value match {
			case "b" =>
				d_label.text = d_label.value
			case "0" =>
				d_label.text = ""
			case _   =>
				d_label.text = d_label.value
				d_label.foreground = DGE.demineur_color_list(d_label.text.toInt)
		}
	}
}

class Label_State_Flagged extends Demineur_Label_State {
	val state_name = "flagged"

	val label_border = DGE.border(DGE.black)
	val background = DGE.label_color_flagged
	val text = ""
	def f_custom_painting (g:Graphics2D, label:Label) = {
		if (Demineur.color_parameter == "Ocean") {
			val center = ((label.size.width/2).toInt, ((label.size.height/2).toInt))
			g.setColor(DGE.green)
			val radius = 15
			g.fillOval(center._1-(radius/2),center._2-(radius/2),radius,radius)
		}		
	}
	custom_painting = f_custom_painting
}

trait Demineur_Label_States_Manager {
	val Label_State_Unexplored = new Label_State_Unexplored
	val Label_State_Explored = new Label_State_Explored
	val Label_State_Flagged = new Label_State_Flagged

	def change_to_state(d_label: Demineur_Label, state_name: String) = {
		d_label.state = state_name
		state_name match {
			case Label_State_Unexplored.state_name => Label_State_Unexplored.change_to_state(d_label)
			case Label_State_Explored.state_name => Label_State_Explored.change_to_state(d_label)
			case Label_State_Flagged.state_name => Label_State_Flagged.change_to_state(d_label)
		}
	}
}

class Demineur_Label extends Grid_Label with Demineur_Label_States_Manager /*with Demineur_Graphical_Elements*/{
	var state = "unexplored" //valeur nécessaire pour que les Demineurs_Label puissent etre instanciés par Grid main, inutile sinon
	var discovered = false
	var flag = false
	var value = "?"
	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
	preferredSize = new Dimension(Demineur.square_size_x, Demineur.square_size_y)

	init()

	def init() : Unit = {
		change_to_state(this,"unexplored")
		discovered = false
		flag = false
		text = ""
		listenTo(mouse.moves, mouse.clicks)
	}

	override def mouse_enter_reaction () ={
		if (!discovered)
			border = DGE.highlighted_border
	}
	override def mouse_exit_reaction () ={
		if (!discovered)
			border = DGE.border(DGE.black)
	}
	override def mouse_leftclic_reaction () ={
		if (!flag)
			discover()
	}
	override def mouse_rightclic_reaction () = {
		flag_unflag()
	}
	
	def flag_unflag() : Unit = {
		if (!discovered) { //Pas utile car seuls les label non découverts écoutent les clics de souris, mais plus clair
			if (flag) {
				change_to_state(this,"unexplored")
				Demineur.maj_nb_flag(-1)
				flag = false
			}
			else {
				change_to_state(this,"flagged")
				Demineur.maj_nb_flag(1)
				flag = true
			}
		}
	}

	def discover() : Unit = {
		if (!discovered) {	//pas utile car seuls les label non découverts écoutent les clics de souris, mais plus clair
			deafTo(mouse.moves, mouse.clicks)
			discovered = true
			Demineur.increment_nb_discovered_square()
			if (value == "?") {//ie ce label est le premier à etre cliqué dans cette partie
				Demineur.place_bombs(numero)
				Demineur.game_frame_content.timer_label.restart(new Date()) //lance le timer au premier clic sur une case de l'utilisateur
			}
			change_to_state(this,"explored")
			value match {
				case "b" =>
					text = "X"
					background = DGE.red
					Demineur.lose()
				case "0" =>
					text = ""
					Demineur.spread(numero)
				case _   =>
					text = value
					foreground = DGE.demineur_color_list(text.toInt)
			}
		}
	}
	
}
