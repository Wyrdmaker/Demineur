import scala.swing._
import scala.swing.event._

abstract class Demineur_Label_State extends Label_State[Demineur_Label] with Demineur_Graphical_Elements {
	val size_x = Demineur.square_size_x
	val size_y = Demineur.square_size_y
	val opaque = true
	val foreground = black
}

class Label_State_Unexplored extends Demineur_Label_State{
	val state_name = "unexplored"

	val label_border = black_border
	val background = label_color_unexplored
	val text = ""
}

class Label_State_Explored extends Demineur_Label_State {
	val state_name = "explored"

	val label_border = black_dim_border
	val background = label_color_explored
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
				d_label.foreground = Demineur.demineur_color_list(d_label.text.toInt)
		}
	}
}

class Label_State_Flagged extends Demineur_Label_State {
	val state_name = "flagged"

	val label_border = black_border
	val background = label_color_flagged
	val text = ""
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

class Demineur_Label extends Grid_Label with Demineur_Label_States_Manager with Demineur_Graphical_Elements{
	var state = "unexplored"
	var discovered = false
	var flag = false
	var value = "?"
	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
	init()

	def init() : Unit = {
		change_to_state(this,"unexplored")
		discovered = false
		flag = false
		text = ""
		listenTo(mouse.moves, mouse.clicks)
	}

	reactions += {
        case e : MouseEntered =>
			if (!discovered) 
            	border = blue_border
        case e : MouseExited =>
			if (!discovered)
          		border = black_border
		case e : MouseClicked =>
			if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1 && !flag)
				discover()
			else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3)
				flag_unflag()
	}
	
	def flag_unflag() : Unit = {
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

	def discover() : Unit = {
		if (!discovered) {
			deafTo(mouse.moves, mouse.clicks)
            discovered = true
			Demineur.increment_nb_discovered_square()
			if (value == "?")
				Demineur.place_bombs(numero)
			change_to_state(this,"explored")
			value match {
				case "b" =>
					text = "X"
					background = red
					Demineur.lose()
				case "0" =>
					text = ""
					Demineur.spread(numero)
				case _   =>
					text = value
					foreground = Demineur.demineur_color_list(text.toInt)
			}
		}
	}
	
}