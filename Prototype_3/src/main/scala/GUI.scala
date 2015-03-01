import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

//Une Bonne Idée mais ça demande du boulot
//Demineur extends Game
/*
class GUI(game: Game) {
	//new Array[game.Game_Label]

}
*/

trait Colors {

	val black = new Color(0,0,0,255)
	val black_dim = new Color(0,0,0,50)

	val white = new Color(255,255,255)
	val blue = new Color(0,0,255)
	val green = new Color(0,200,0)
	val red = new Color(255,0,0)
	val cyan = new Color(0,255,255)
	val purple = new Color(150,0,175)
	val light_green = new Color(50,200,120)
	val light_brown = new Color(200,120,50)
	val color9 = new Color(0,200,200)
}

trait Label_Borders extends Colors{
	// Le deuxième paramétre de Swing.LineBorder est l'épaisseur
	val black_border = Swing.LineBorder(black,1)
	val black_dim_border = Swing.LineBorder(black_dim,1)
	val blue_border = Swing.LineBorder(blue,1)
}



abstract class Difficulty_Mode {
	def set_game_parameters () :Unit
	val mode_name : String
}



abstract class Game{
	val title: String
	type Game_Label_Class <: Grid_Label
	def glb_factory () : Game_Label_Class
	val square_size_x: Int
	val square_size_y: Int
	var nb_of_rows: Int
	var nb_of_cols: Int
	var game_beginning_time: Date
	def about_frame_factory (): Frame
	def help_frame_factory (): Frame
	def game_starter (): Unit // game_starter ne contient que les choses à faire avant de lancer une partie qui sont spécifiques au jeu, le reste est fait dans generic_game_starter
	def action_restart (): Unit
	var in_game: Boolean
	var game_frame_content : Game_Frame_Content[Game_Label_Class] = null
	var random_gen = new scala.util.Random()

	type Game_Difficulty_Mode <: Difficulty_Mode
	val game_difficulty_mode_list : IndexedSeq[Game_Difficulty_Mode]
	def game_custom_mode (): Game_Difficulty_Mode
}





//Est ce qu'on pourrait se défaire du paramètrage de Grid avec Game_Label_Class en allant chercher le type Game_Label_Class de game ??
class Grid[Game_Label_Class <: Grid_Label] (game: Game/*, nb_of_cols: Int, nb_of_rows: Int, factory : Unit => Game_Label_Class*/) extends GridPanel(game.nb_of_rows, game.nb_of_cols) /*GridPanel prend le nb de lignes puis le nb de colonnes*/{
	val nb_of_cols = game.nb_of_cols
	val nb_of_rows = game.nb_of_rows
	//Remplir la grille d'objet de la classe Grid_Label
	for (cy<-1 to nb_of_rows) {
		for (cx<- 1 to nb_of_cols) {
			val label = game.glb_factory()
			label.x = cx-1; label.y = cy-1; label.numero = (cy-1)*nb_of_cols +(cx-1);
			contents += {label}
		}
	}

	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access_xy(x: Int, y: Int) ={
		contents(y*nb_of_cols + x).asInstanceOf[Game_Label_Class]
	}
	//Renvoit le label de numéro n
	def access_n(n: Int) ={
		contents(n).asInstanceOf[Game_Label_Class]
	}
	//Renvoit la liste des labels de la grille
	def get_contents() = {
		contents.map((x) => x.asInstanceOf[Game_Label_Class])
	}
}

abstract class Grid_Label extends Label{
	var x = 0
	var y = 0
	var numero = 0
	var state: String
}






class Number_Field(init_string : String) extends TextField(init_string) {
	listenTo(keys)
	reactions += {
		case e : KeyTyped =>
			if (!e.char.isDigit)
				e.consume
	}
}

//les couples d'Int de fields_bounds_list représentent le min et le max que l'utilisateur peut rentrer dans le formulaire ((n,n) avec n un entier signifie pas de limite)
class Number_Form(titre : String, fields_names_list : IndexedSeq[String], fields_bounds_list : IndexedSeq[(Int,Int)]) extends Dialog {
	var result: IndexedSeq[Int] = fields_bounds_list map (couple => couple._1)
	var accepted : Boolean = false
	if (fields_names_list.length == fields_bounds_list.length) {
		title = titre
		//accepted = false
		modal = true
		var number_fields_list = fields_bounds_list map (couple =>
			couple match {
				case (min_value,max_value) => new Number_Field(((max_value + min_value)/2).toString)
			})
		contents = new GridPanel(fields_names_list.length + 1, 2) {
			for (i <- 0 until fields_names_list.length) {
				var bounds_string = "  (" + fields_bounds_list(i)._1 + "/" + fields_bounds_list(i)._2 + ")"
				if (fields_bounds_list(i)._1 == fields_bounds_list(i)._2) { bounds_string = ""}
				contents += new Label(fields_names_list(i) + bounds_string + " : ")
				contents += number_fields_list(i)
			}
			contents += new Label("")
			contents += new Button("") {
				action = Action("Jouer")(submit)
			}
		}

		def submit = {
			
			result = number_fields_list map (number_field => number_field.text.toInt)
			var bound_condition = true
			for (i <- 0 to result.length - 1 ) {
				if (!((fields_bounds_list(i)._1 <= result(i) && result(i) <= fields_bounds_list(i)._2)
					|| fields_bounds_list(i)._1 == fields_bounds_list(i)._2)) {
					bound_condition = false
					number_fields_list(i).text = ((fields_bounds_list(i)._1 + fields_bounds_list(i)._2)/2).toString
				}
			}

			if (bound_condition) {
				accepted = true
				visible = false
			}
			else {
				println("Les réponses aux formulaires ne sont pas dans les bornes définies")
			
			}
			

		}

		visible = true
	}
	else {
		println("Anormal: La classe Number_Form a été instanciée avec deux listes de tailles différentes")
		println(fields_names_list.length)
		println(fields_bounds_list.length)
	}
}





//Idée: écrire un truc pour qu'on puisse dire à une action de s'éxécuter dans n secondes (lance un timer avec timeout puis execute l'action)
class Timer_Label (time_origin_arg : Date) extends Label{
	val this_timer_label = this
	var time_origin = time_origin_arg
	var minutes = ((new Date).getTime() - time_origin.getTime()) / 60000 % 60
	var secondes = ((new Date).getTime() - time_origin.getTime()) / 1000 % 60

	def restart (new_time_origin: Date) = {
		time_origin = new_time_origin
		timer.start()
	}

	def start () = {
		timer.start()
	}

	def stop () = {
		timer.stop()
	}
	
	val timer_listener = new ActionListener{
		def actionPerformed(e: ActionEvent) {
			minutes  = ((new Date).getTime() - time_origin.getTime()) / 60000 % 60
			secondes = ((new Date).getTime() - time_origin.getTime()) / 1000 % 60
			var string = if (minutes < 10) "0" else ""
			string = string + minutes.toString + ":"
			string = if (secondes < 10) string + "0" else string
			string = string + secondes.toString
			this_timer_label.text = string
		}
	}

	val timer = new javax.swing.Timer(1000, timer_listener)

	timer.start()
}

//Créer des formes génériques de demineur_starter, restart, random_seed et des MIM correspondants

class Game_Frame_Content[Game_Label_Class <: Grid_Label] (game: Game/*, label_1_dimension: Dimension, label_2_dimension: Dimension, outcome_label_dimension: Dimension, timer_label_dimension: Dimension*/) {
	val label_1 = new Label()
	//label_1.preferredSize = label_1_dimension

	val label_2 = new Label()
	//label_2.preferredSize = label_2_dimension

	val outcome_label = new Label()
	//outcome_label.preferredSize = outcome_label_dimension

	val timer_label = new Timer_Label(game.game_beginning_time)
	//timer_label.preferredSize = timer_label_dimension

	val grid = new Grid[Game_Label_Class](game)

	val bottom_panel = new FlowPanel() {
		contents += label_1
		contents += label_2
		contents += outcome_label
		contents += timer_label
		preferredSize = new Dimension(300,30)
	}

	val border_panel = new BorderPanel {
		layout(grid) = North
		layout(bottom_panel) = South
	}

	val final_content = border_panel
}

//Les états de label sont des set de paramètres graphiques
abstract class Label_State[Game_Label_Class <: Grid_Label] {
	val state_name: String

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

case class Custom_Mode_Exception(value: String) extends Throwable{}

class UI (game: Game) extends MainFrame {
	val thisui = this
	title = game.title
	resizable = false
	contents = new Label("Welcome ! ;)"){
		preferredSize = new Dimension(300,300)
	}
	val Game_Starter = new Generic_Game_Starter(game,thisui)
	val Action_Restart = new Generic_Action_Restart(game)

	def action_generic_random_seed() {
		if (game.game_frame_content != null){
			var random_seed_form = new Number_Form(
				"Random Seed",
				IndexedSeq("Random Seed"),
				IndexedSeq((0,0))
				)
			val asked_random_seed = random_seed_form.result(0)
			game.random_gen = new scala.util.Random(asked_random_seed)

			Game_Starter.generic_game_starter()
		}	
	}

	def action_generic_custom_mode()  {
		try {
			val custom_difficulty_mode = game.game_custom_mode()
			custom_difficulty_mode.set_game_parameters()
			Game_Starter.generic_game_starter()
		}
		catch {
			case e: Custom_Mode_Exception => ();
		}
	}

	class Playmenu_MIM(difficulty_mode: Difficulty_Mode) extends MenuItem(""){
		def menuitem_action () = {
			difficulty_mode.set_game_parameters()
			Game_Starter.generic_game_starter()
		}
		action = Action(difficulty_mode.mode_name)(menuitem_action)
	}

	menuBar = new MenuBar {
		contents += new Menu("Play") {	
			game.game_difficulty_mode_list.foreach(difficulty_mode =>
				contents += new Playmenu_MIM(difficulty_mode)
			)
			contents += new MenuItem("")
			contents += new MenuItem(""){action = Action("Custom...")(action_generic_custom_mode())}
		}
		contents += new Menu("Game") {
			//contents += new MenuItem(""){action = Action("New")()}
			contents += new MenuItem(""){action = Action("Restart")(Action_Restart.action_restart())}
			contents += new MenuItem(""){action = Action("Random Seed...")(action_generic_random_seed())}
		}
		contents += new Menu("Help") {
			contents += new MenuItem(""){action = Action("About")(game.about_frame_factory())}
			contents += new MenuItem(""){action = Action("Help on " + game.title)(game.help_frame_factory())}
		}
	}

class Generic_Action_Restart (game: Game) {
	def action_restart() ={
		if (game.game_frame_content != null) {
			game.action_restart()
			val outcome_label = game.game_frame_content.outcome_label
			outcome_label.text = ""

			game.game_beginning_time = new Date()
			game.in_game = true

			val timer_label = game.game_frame_content.timer_label
			timer_label.restart(game.game_beginning_time)
		}
	}
}

class Generic_Game_Starter (game: Game, ui: Frame) {
	def generic_game_starter (): Unit ={
		game.action_restart()


		game.game_beginning_time = new Date()

		val game_frame_content = new Game_Frame_Content[game.Game_Label_Class](game)
		game.game_frame_content = game_frame_content
		ui.contents = game_frame_content.final_content

		game_frame_content.timer_label.restart(new Date())
		game.game_starter()
		game.in_game = true
	}
}





}





