import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}

//import javax.swing.{ImageIcon, Icon}

//Les états de label sont des set de paramètres graphiques
abstract class Label_State[Game_Label_Class <: Grid_Label] {
	val size_x: Int
	val size_y: Int
	val label_border: javax.swing.border.Border
	val opaque: Boolean
	val background: Color
	val foreground: Color
	val text: String

	def change_to_state(game_label: Game_Label_Class) = {
		game_label.preferredSize = new Dimension(size_x,size_y)
		game_label.border = label_border
		game_label.opaque = opaque
		game_label.background = background
		game_label.foreground = foreground
		game_label.text = text
	}
} 

abstract class Demineur_Label_State extends Label_State[Demineur_Label] with Demineur_Graphical_Elements {
	val size_x = Demineur.square_size_x
	val size_y = Demineur.square_size_y
	val opaque = true
	val foreground = black
}

class Label_State_Unexplored extends Demineur_Label_State{
	val label_border = black_border
	val background = label_color_unexplored
	val text = ""
}

class Label_State_Explored extends Demineur_Label_State {
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
			case "unexplored" => Label_State_Unexplored.change_to_state(d_label)
			case "explored" => Label_State_Explored.change_to_state(d_label)
			case "flagged" => Label_State_Flagged.change_to_state(d_label)
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
					text = value
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

trait Demineur_Colors extends Colors {
	val label_color_unexplored = new Color(255,100,0)
	val label_color_explored = new Color(255,200,100)
	val label_color_flagged = new Color(255,50,50)

}

trait Demineur_Graphical_Elements extends Demineur_Colors with Label_Borders {
	val demineur_color_list = List (
		white,
		blue,
		green,
		red,
		cyan,
		purple,
		light_green,
		light_brown
	)
}
/*
trait Demineur_Parameters {
	val square_size_x = 35 //éventuellement modifiées (une autre valeur est utilisée à la place) par Demineur_Frame_Content s'il y a trop peu de cases pour que end_label, timer_label et flag_nb_label ait la place de s'afficher correctement
	val square_size_y = 35 //idem
	var square_dimension = new Dimension(square_size_x,square_size_y)

}
*/

object Demineur extends Game with Demineur_Graphical_Elements{
	val square_size_x = 35
	val square_size_y = 35
	type Game_Label_Class = Demineur_Label
	def glb_factory () ={new Game_Label_Class } // "glb" -> "Game_Label_Class"
	val title = "Démineur"
	var nb_discovered_square = 0
	var nb_flagged_square = 0
	var nb_of_bombs = 0
	var in_game = false
	var nb_of_rows = 0
	var nb_of_cols = 0
	var game_beginning_time: Date = null
	var grid:Grid[Game_Label_Class] = null
	var demineur_frame_content: Demineur_Frame_Content = null

	//GAME FUNCTIONS
	def increment_nb_discovered_square() = {
		nb_discovered_square += 1
		if (nb_discovered_square + nb_of_bombs == nb_of_rows * nb_of_cols)
			win()
	}

	def neighbour(n : Int) : List[Int] = {
		var lst : List[Int]= List()
		var a = if (n % nb_of_cols == 0) 0 else -1 //bord gauche du carré
        var b = if (n % nb_of_cols == nb_of_cols - 1) 0 else 1 //bord droit du carré
        var c = if (n < nb_of_cols) 0 else -1 // bord haut du carré
        var d = if (n >= (nb_of_rows - 1) * nb_of_cols) 0 else 1 // bord bas du carré
        for (i <- a to b) {
            for (j <- c to d) {
                if (0 <= n + j * nb_of_cols + i && n + j * nb_of_cols + i < nb_of_rows * nb_of_cols) {
                    lst ++= List(n + j * nb_of_cols + i) // LOLILOOOL
                }
            }
        }
		return lst	
	}

	def maj_nb_flag(n : Int /*normalement 1, -1 ou 0*/) = {
		n match {
			case 1 => nb_flagged_square = nb_flagged_square + n 
			case -1 => nb_flagged_square = nb_flagged_square + n
			case 0 => nb_flagged_square = nb_flagged_square + n
			case _ => println("anormal: la fonction maj_nb_flag de l'objet Demineur a été appelée avec un argument différent de 1, -1 ou 0:" + n)

		}
		val flag_nb_label = demineur_frame_content.flag_nb_label
		flag_nb_label.text = "B : " + nb_flagged_square.toString + " / " + nb_of_bombs.toString
		if (nb_flagged_square > nb_of_bombs)
			flag_nb_label.foreground = label_color_flagged
		else
			flag_nb_label.foreground = new Color(0,0,0)
	}

	def place_bombs(n_origin_label : Int) = {
		val grid = demineur_frame_content.grid
		
		var bombs_left = nb_of_bombs
		var random_gen = scala.util.Random
		neighbour(n_origin_label).foreach(n => grid.access_n(n).value = "#")
		while (bombs_left > 0) {
			var random = random_gen.nextInt(nb_of_rows * nb_of_cols)

			if (grid.access_n(random).value == "?") {
				grid.access_n(random).value = "b"

				bombs_left -= 1
			}
		}
		
		val grid_label_list = grid.get_contents
		
		grid_label_list.foreach(label => 
			if (label.value != "b"){
				var new_value = 0
				neighbour(label.numero).foreach(number => 
					if (grid.access_n(number).value == "b") {new_value += 1}
				)
				label.value = new_value.toString
			}
		)

	}

	def win() = {
		val end_label = demineur_frame_content.end_label
		val timer_label = demineur_frame_content.timer_label
		val grid_content = demineur_frame_content.grid.get_contents
		timer_label.stop()
		in_game = false
        end_label.text = "WIN !"
        end_label.background = new Color(0,255,0)
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))
		
	}

	def lose() = {
		val end_label = demineur_frame_content.end_label
		val timer_label = demineur_frame_content.timer_label
		val grid_content = demineur_frame_content.grid.get_contents
		timer_label.stop()
		in_game = false
		end_label.text = "GAME OVER !"
		end_label.background = new Color(255,0,0)
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))
	}

	def spread(numero : Int) = {
		val grid_content = demineur_frame_content.grid.get_contents
		var voisins_list = neighbour(numero)
		voisins_list.foreach(numero => grid_content(numero).discover())
		
	}

	//MENU FUNCTIONS
	def regenerate (frame: Frame) {
		if (Demineur.demineur_frame_content != null){
			demineur_starter(frame, Demineur.nb_of_cols, Demineur.nb_of_rows, Demineur.nb_of_bombs)	
		}

	}
		/*"MIM" -> "Menu Item Maker"*/
	class MIM_Regenerate(frame: Frame) extends MenuItem(""){
		def action_regenerate () :Unit ={
			Demineur.regenerate(frame)
		}
		action = Action("Random seed")(action_regenerate)
	}

	def demineur_starter(frame: Frame,nb_of_cols: Int, nb_of_rows: Int,nb_of_bombs: Int) = {		
			Demineur.action_restart //Pour le cas où l'utilisateur lance d'autres parties que la première -> remet à 0 flag_nb_label et end_label (en particulier)

			Demineur.game_beginning_time = new Date()
			Demineur.nb_of_rows = nb_of_rows
			Demineur.nb_of_cols = nb_of_cols
			Demineur.nb_of_bombs = nb_of_bombs

			var demineur_frame_content = new Demineur_Frame_Content(Demineur)
			Demineur.demineur_frame_content = demineur_frame_content
			Demineur.maj_nb_flag(0)

			Demineur.in_game = true
			frame.contents = demineur_frame_content.final_content		
	}

	class MIM_Demineur_Starter(frame: Frame,nb_of_cols: Int, nb_of_rows: Int,nb_of_bombs: Int) extends MenuItem(""){
		def action_demineur_starter () : Unit= {
			Demineur.demineur_starter(frame: Frame,nb_of_cols: Int, nb_of_rows: Int,nb_of_bombs: Int)
		}
		action = Action("Grille "+nb_of_cols+"*"+nb_of_rows+", "+nb_of_bombs+" bombes")(action_demineur_starter)
	}

	def custom_grid_demineur_starter (frame : Frame) = {

		
		var custom_grid_form = new Number_Form(
			"Grille Perso",
			IndexedSeq("x", "y",  "b"),
			IndexedSeq((4,20), (4,20), (10,10))
		)
		val form_result = custom_grid_form.result
		val asked_nb_of_cols = form_result(0)
		val asked_nb_of_rows = form_result(1)
		val asked_nb_of_bombs = form_result(2)
		if (	custom_grid_form.accepted 
			&& 	asked_nb_of_cols * asked_nb_of_rows > 9 
			&& 	asked_nb_of_bombs + 9 <= asked_nb_of_cols * asked_nb_of_rows) {
			demineur_starter(frame,asked_nb_of_cols,asked_nb_of_rows,asked_nb_of_bombs)
		}
		else {
			println("Les réponses au formulaire ne permettent pas de créer une grille convenable")
		}
	}
	
	class MIM_Custom_Grid_Demineur_Starter(frame: Frame) extends MenuItem(""){
		def action_custom_grid_game_starter() :Unit ={
			Demineur.custom_grid_demineur_starter(frame)
		}
		action = Action("Grille personalisée")(action_custom_grid_game_starter)
	}

	def action_restart() : Unit = {
		if (Demineur.demineur_frame_content != null) {
			val grid_contents = Demineur.demineur_frame_content.grid.get_contents
			grid_contents.foreach(label => label.init())

			val end_label = Demineur.demineur_frame_content.end_label
			end_label.text = ""

			Demineur.nb_discovered_square = 0
			Demineur.nb_flagged_square = 0
			Demineur.maj_nb_flag(0)
			Demineur.game_beginning_time = new Date()
			Demineur.in_game = true

			val timer_label = Demineur.demineur_frame_content.timer_label
			timer_label.restart(Demineur.game_beginning_time)
		}
	}

}


//Crée le contenu de la fenetre de jeu
class Demineur_Frame_Content (game: Game) {

	val end_label = new Label()
	end_label.preferredSize = new Dimension(math.max(game.nb_of_cols * game.square_size_x / 3,3*35),30)

	val flag_nb_label = new Label()
	flag_nb_label.preferredSize = new Dimension(math.max(game.nb_of_cols * game.square_size_x / 3,2*35),30)

	val timer_label = new Timer_Label(game.game_beginning_time)
	timer_label.preferredSize = new Dimension(math.max(game.nb_of_cols * game.square_size_x / 3,2*35),30)

	val grid = new Grid[Demineur_Label](game.nb_of_cols,game.nb_of_rows,unit => new Demineur_Label )

	val bottom_panel = new FlowPanel() {
		//Labels
		contents += flag_nb_label
		contents += end_label
		contents += timer_label

	}

	val border_panel = new BorderPanel {
		layout(grid) = North
		layout(bottom_panel) = South
	}

	val final_content = border_panel

}




class UI extends MainFrame with Demineur_Colors{
	val thisui = this
	title = Demineur.title
	resizable = false
	contents = new Label("Welcome ! ;)"){
		preferredSize = new Dimension(300,300)
	}
	menuBar = new MenuBar {
                contents += new Menu("Game") {               		
                    contents += new Demineur.MIM_Demineur_Starter(thisui,9,9,10)
                    contents += new Demineur.MIM_Demineur_Starter(thisui,16,16,40)
                    contents += new Demineur.MIM_Demineur_Starter(thisui,16,16,99)
                    contents += new MenuItem(""){action = Action("Restart")(Demineur.action_restart)}
                    contents += new Demineur.MIM_Regenerate(thisui)
                    contents += new Demineur.MIM_Custom_Grid_Demineur_Starter(thisui)
                }
                contents += new Menu("About") {
                	contents += new MenuItem(""){action = Action("Mysterious")(println("indeed !"))}
                }
    }

}


object GuiProgram {
	def main(args: Array[String]) {
		val ui = new UI
		ui.visible = true
	}
}
