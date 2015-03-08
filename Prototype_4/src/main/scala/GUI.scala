import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
import scala.swing.ComboBox
//import javax.swing.{ImageIcon, Icon}


trait GUI_Colours {
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

	val violet_fluo = new Color(255,0,255)
	val bleu_clair = new Color(50,205,255)
}

trait Label_Borders extends GUI_Colours{
	// Le deuxième paramètre de Swing.LineBorder est l'épaisseur
	val black_border = Swing.LineBorder(black,1)
	val black_dim_border = Swing.LineBorder(black_dim,1)
	val blue_border = Swing.LineBorder(blue,1)
}
//Chaque jeu doit l'instancier avec deux tableaux de valeurs correspondant aux valeurs de paramètres de jeu numériques et textuels 
//pour définir les modes de difficulté qu'il souhaite proposer et les regrouper dans la variable game_difficulty_mode_list sous la forme d'une IndexedSeq
case class Difficulty_Mode(numeric_game_parameters_values_list: IndexedSeq[Int], string_game_parameters_values_list: IndexedSeq[String]) {
	def set_game_parameters (game: Game) ={
		if (numeric_game_parameters_values_list.length == game.numeric_game_parameters_def_list.length) {
			for (i <- 0 until numeric_game_parameters_values_list.length) {
				//Modifie le champ correspondant à la valeur du paramètre numérique i du jeu, en lui assignant la ième valeur 
				//de la liste de valeurs pour les paramètres numérique du jeu du mode de difficulté
				Game_Parameters_Value_Setters.numeric_game_parameter_value_setter(i, numeric_game_parameters_values_list(i), game)
			}
		}
		else {println("Anormal: le nombre de valeurs pour les paramètres numériques du jeu déclarées dans un certain mode de difficulté n'est pas égal au nombre de paramètres numériques de ce jeu")}
		if (string_game_parameters_values_list.length == game.string_game_parameters_def_list.length) {
			for (i <- 0 until string_game_parameters_values_list.length) {
				//Modifie le champ correspondant à la valeur du paramètre textuel i du jeu, en lui assignant la ième valeur 
				//de la liste de valeurs pour les paramètres textuels du jeu du mode de difficulté
				Game_Parameters_Value_Setters.string_game_parameter_value_setter(i, string_game_parameters_values_list(i), game)
			}
		}
		else {println("Anormal: le nombre de valeurs pour les paramètres textuels du jeu déclarées dans un certain mode de difficulté n'est pas égal au nombre de paramètres textuels de ce jeu")}

	}
}

object Game_Parameters_Value_Setters {
	//Cette méthode sert à modifier le champ correspondant à la valeur du paramètre numérique numéro no_parameter du jeu game, en lui assignant la valeur new_value
	def numeric_game_parameter_value_setter (no_parameter: Int, new_value: Int, game: Game) = {
		val old_numeric_parameter_def = game.numeric_game_parameters_def_list(no_parameter)
		var new_numeric_parameter_def = ("", 0, 0, 0)
		old_numeric_parameter_def match {
			case (parameter_name, parameter_value, inf_bound, sup_bound) =>
				new_numeric_parameter_def = (parameter_name, new_value, inf_bound, sup_bound)
		}
		game.numeric_game_parameters_def_list = game.numeric_game_parameters_def_list.updated(no_parameter, new_numeric_parameter_def)
		 	//la méthode updated renvoie une autre liste identique à la première dans 
			//laquelle le ième terme à été remplacé par le terme donné en deuxième 
			//argument. On en a besoin ici car les IndexedSeq de types combiné 
			//(ex: [Int,Int,IndexedSeq[String]]) sont immutables et donc non 
			//modifiables de façon classique
	}
	//Cette méthode sert à modifier le champ correspondant à la valeur du paramètre textuel numéro no_parameter du jeu game, en lui assignant la valeur new_value
	def string_game_parameter_value_setter (no_parameter: Int, new_value: String, game: Game) = {
		val old_string_parameter_def = game.string_game_parameters_def_list(no_parameter)
		var new_string_parameter_def = ("", "", IndexedSeq(""))
		old_string_parameter_def match{
			case (parameter_name, parameter_value, parameter_possible_values) =>
				new_string_parameter_def = (parameter_name, new_value, parameter_possible_values)
		}
		game.string_game_parameters_def_list = game.string_game_parameters_def_list.updated(no_parameter,new_string_parameter_def)
			//la méthode updated renvoie une autre liste identique à la première dans 
			//laquelle le ième terme à été remplacé par le terme donné en deuxième 
			//argument. On en a besoin ici car les IndexedSeq de types combiné 
			//(ex: [Int,Int,IndexedSeq[String]]) sont immutables et donc non 
			//modifiables de façon classique
	}
}

//Signature d'un jeu
abstract class Game extends GUI_Colours{
	val title: String
	val square_size_x: Int 	//largeur des cases
	val square_size_y: Int	//hauteur des cases

	var game_beginning_time: Date //date de début de la partie pour le chronomètre
	var in_game = false
	var numeric_game_parameters_def_list: IndexedSeq[(String, Int, Int, Int)]		//liste des paramètres numériques du jeu sous la forme de tuples 
																				//(nom, valeur, borne_inf_pour_mode_custom, borne_sup_pour_mode_custom) et dont les deux 
																				//premiers doivent etre "Width" et "Height" (resp le nb de colonnes de la grille et 
																				//le nb de lignes de la grille)
	var string_game_parameters_def_list: IndexedSeq[(String, String, IndexedSeq[String])]	//liste des paramètres chaines de caractères du jeu sous la forme de tuples 
																							//(nom, valeur, IndexedSeq_des_valeurs_possibles_de_ce_paramètre)

	type Game_Label_Class <: Grid_Label	//Les labels avec lesquels sera remplis la grille (par la classe Grid)
	def glb_factory () : Game_Label_Class	//Une usine à labels de la classe Game_Label_Class
	def about_frame_factory (): Frame 		//une fonction qui fournit la fenetre "About" du jeu
	def help_frame_factory (): Frame 		//une fonction qui fournit la fenetre "Help" du jeu

	var random_gen = new scala.util.Random()	//Le générateur aléatoire utilisé par le jeu
	var game_frame_content : Game_Frame_Content[Game_Label_Class] = null 	//Variable stockant le contenu graphique de la fenetre de jeu lors d'une partie

	val game_difficulty_mode_list : IndexedSeq[Difficulty_Mode] 	//Liste des modes de difficulté que le jeu veut proposer
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]): String	//Une fonction qui, aux résultat des champs numériques d'un formulaire
																							// de partie custom, vérifie des conditions propres au jeu 
																							//(si ces paramètres permettent de jouer au jeu ou non) et 
																							//renvoie "OK" si les valeurs reçus vérifient ces conditions et une 
																							//string contenant le message d'erreur sinon
	def game_starter (): Unit 	// game_starter ne contient que les choses à faire avant de lancer une partie qui sont spécifiques au jeu, le reste 
								//est fait dans generic_game_starter
	def game_action_restart (): Unit 	//game_action_restart ne contient que les choses à faire avant de relancer une partie qui sont spécifique au jeu, le reste 
										//est fait dans generic_action_restart

	def win() = {
		in_game = false
		val outcome_label = game_frame_content.outcome_label
		val timer_label = game_frame_content.timer_label
		val grid_content = game_frame_content.grid.get_contents
		outcome_label.text = "WIN !"
		outcome_label.background = green
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

/*Inutile Ici, conservé pour références futures
//Une exception lancée par la fonction game_custom_mode d'un jeu lorsque les paramètres numériques renvoyés par le formulaire ne permettent pas de créer une partie du jeu
case class Custom_Mode_Exception(value: String) extends Throwable{}
*/

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
		val nb_fields_def_list = game.numeric_game_parameters_def_list map (numeric_game_parameter_def => 
			numeric_game_parameter_def match {
				case (parameter_name, parameter_value, parameter_inf_bound, parameter_sup_bound) =>
					(parameter_name, parameter_inf_bound, parameter_sup_bound)
			}
		)
		val comboboxes_def_list = game.string_game_parameters_def_list map (string_game_parameter =>
			string_game_parameter match {
				case (parameter_name, parameter_value, parameter_possible_values) => 
					(parameter_name, parameter_possible_values)
			}
		)
			val custom_game_form = new Form(
				"Custom Game",
				nb_fields_def_list,
				comboboxes_def_list,
				game.custom_game_parameters_conditions)
			val form_nb_fields_results = custom_game_form.nb_fields_results		//Les résultats numériques du formulaire
			val form_comboboxes_results = custom_game_form.comboboxes_results	//Les résultats textuels du formulaire	 
			var custom_difficulty_mode = Difficulty_Mode(form_nb_fields_results, form_comboboxes_results)
			if (custom_game_form.form_accepted) {	// Vérifie que le formulaire soit accepté 
													//(en pratique, ça attend que le joueur ait cliqué
													// sur le bouton fini),
				custom_difficulty_mode.set_game_parameters(game)
				Game_Starter.generic_game_starter()
			}
			else {println("le formulaire a été fermé")}
	}
	//"MIM" signifie "MenuItemMaker"
	class Playmenu_MIM(difficulty_mode: Difficulty_Mode) extends MenuItem(""){ //Fabrique un élément du menu Play à partir de l'un des modes de difficulté spécifiés par le jeu
		def menuitem_action () = {
			difficulty_mode.set_game_parameters(game)
			Game_Starter.generic_game_starter()
		}
		var difficulty_mode_name = ""
		difficulty_mode match { // Ce gros bloc sert à remplir la variable difficulty_mode_name avec le nom du mode de difficulté, obtenu en mettant bout à bout
								// les différents paramètres de jeu qui constitue le mode de difficulté
			case Difficulty_Mode(numeric_game_parameters_values_list: IndexedSeq[Int], string_game_parameters_values_list: IndexedSeq[String]) =>
				difficulty_mode_name = numeric_game_parameters_values_list(0).toString + "x" + numeric_game_parameters_values_list(1).toString
				if (numeric_game_parameters_values_list.length > 2) {
					for (i <- 2 until numeric_game_parameters_values_list.length) {
						difficulty_mode_name += ", " + numeric_game_parameters_values_list(i).toString + " " + game.numeric_game_parameters_def_list(i)._1
						//Rajoute ", <nom_du_paramètre_numérique> <valeur_du_paramètre_numérique>" au nom du mode de difficulté
					}
				}
				if (string_game_parameters_values_list.length > 0) {
					for (i <- 0 until string_game_parameters_values_list.length) {
						difficulty_mode_name += ", " + string_game_parameters_values_list(i)
						//Rajoute ", <valeur_du_paramètre_textuel>" au nom du mode de difficulté
					}
				}
		}
		action = Action(difficulty_mode_name)(menuitem_action)
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
			contents += new MenuItem(""){action = Action("Restart")(Action_Restart.action_restart())}
			contents += new MenuItem(""){action = Action("Random Seed...")(action_generic_random_seed())}
			contents += new MenuItem("")
			contents += new MenuItem(""){action = Action("Exit") {System.exit(0)}}
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

