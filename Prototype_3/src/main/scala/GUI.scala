import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

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
	// Le deuxième paramètre de Swing.LineBorder est l'épaisseur
	val black_border = Swing.LineBorder(black,1)
	val black_dim_border = Swing.LineBorder(black_dim,1)
	val blue_border = Swing.LineBorder(blue,1)
}
//Chaque jeu doit en créer uns sous-classe de type case class dont les constructeurs sont les paramètres définissant une partie du jeu.
abstract class Difficulty_Mode {
	//Doit modifier les variables paramètres du jeu pour qu'elles correspondent à celles du Difficulty_Mode
	def set_game_parameters () :Unit 
	//Le nom du mode de difficulté
	val mode_name : String
}

//Signature d'un jeu
abstract class Game extends Colors{
	val title: String
	val square_size_x: Int 	//taille des cases en abscisse
	val square_size_y: Int	//taille des cases en ordonnée
	var nb_of_rows: Int		//nombre de lignes de la grille
	var nb_of_cols: Int		//nombre de colonnes de la grille
	var game_beginning_time: Date //date de début de la partie pour le chronomètre
	var in_game = false

	type Game_Label_Class <: Grid_Label	//Les labels avec lesquels sera remplis la grille (par la classe Grid)
	def glb_factory () : Game_Label_Class	//Une usine à labels de la classe Game_Label_Class
	def about_frame_factory (): Frame 		//une fonction qui fournit la fenetre "About" du jeu
	def help_frame_factory (): Frame 		//une fonction qui fournit la fenetre "Help" du jeu

	var random_gen = new scala.util.Random()	//Le générateur aléatoire utilisé par le jeu
	var game_frame_content : Game_Frame_Content[Game_Label_Class] = null 	//Variable stockant le contenu graphique de la fenetre de jeu lors d'une partie

	type Game_Difficulty_Mode <: Difficulty_Mode 		//Voir la classe Difficulty_Mode de ce fichier
	val game_difficulty_mode_list : IndexedSeq[Game_Difficulty_Mode] 	//Liste des modes de difficulté que le jeu veut proposer
	def game_custom_mode (): Game_Difficulty_Mode 	//une fonction qui doit ouvrir un formulaire, verifier eventuellemnt des conditions sur les résultats (autres que les vérifications faite par Number_Form) et
													// renvoyer un objet de la classe Game_Difficulty_Mode construit avec les réponses


	def game_starter (): Unit // game_starter ne contient que les choses à faire avant de lancer une partie qui sont spécifiques au jeu, le reste est fait dans generic_game_starter
	def game_action_restart (): Unit //game_action_restart ne contient que les choses à faire avant de relancer une partie qui sont spécifique au jeu, le reste est fait dans generic_action_restart
	def win() = {
		in_game = false
		val outcome_label = game_frame_content.outcome_label
		val timer_label = game_frame_content.timer_label
		val grid_content = game_frame_content.grid.get_contents
		outcome_label.text = "WIN !"
		outcome_label.background = new Color(0,255,0)
		timer_label.stop()
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))
	}
	def lose() = {
		in_game = false
		val outcome_label = game_frame_content.outcome_label
		val timer_label = game_frame_content.timer_label
		val grid_content = game_frame_content.grid.get_contents
		outcome_label.text = "GAME OVER !"
		outcome_label.background = red
		timer_label.stop()
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))		
	}
}

//Crée le contenu de la fenetre de jeu (labels du bandeau inférieur et grille)
class Game_Frame_Content[Game_Label_Class <: Grid_Label] (game: Game) {
	val label_1 = new Label()
	val label_2 = new Label()
	val outcome_label = new Label()
		outcome_label.opaque = true
	val timer_label = new Timer_Label(game.game_beginning_time)
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

//Une exception lancée par la fonction game_custom_mode d'un jeu lorsque les paramètres renvoyés par le formulaire ne satisfont pas certaines conditions
case class Custom_Mode_Exception(value: String) extends Throwable{}

//UI est la fenetre principale des jeux
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
	//"MIM" signifie "MenuItemMaker"
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
			game.game_action_restart()
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
		game.game_action_restart()


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





