import scala.swing._
import scala.swing.event._
//import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import scala.math._
//import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

//"DGE" -> "Demineur_Graphical_Element"
object DGE extends GUI_Graphical_Elements{
	def no_color_mode () = {
		//Le max est une sécurité. Si IndexOf ne trouve pas la chaine correspondant au mode de couleur dans la liste de ses valeurs possibles, il renvoie -1.
		//Ainsi, en cas de faute de frappe, le mode de couleur utilisé est le Normal
		max(0,Demineur.string_game_parameters_def_list(1)._3.indexOf(Demineur.string_game_parameters_def_list(1)._2))
	}

	def label_color_unexplored () = {
		label_color_unexplored_list(no_color_mode())
	}
	def label_color_explored () ={
		label_color_explored_list(no_color_mode)
	}
	def label_color_flagged () ={
		label_color_flagged_list(no_color_mode)
	}

	def highlighted_border () ={
		highlighted_border_list(no_color_mode)
	}

	val label_color_unexplored_list = IndexedSeq(new Color(255,100,0), new Color(255, 0, 255), green, new Color(205,51,51), new Color(0,0,190))
	val label_color_explored_list = IndexedSeq(new Color(255,200,100), new Color(65,65,65), red, new Color(139,69,19), new Color(30, 144, 255))
	val label_color_flagged_list = IndexedSeq(new Color(255,50,50), cyan, blue, new Color(255,127,0), /*new Color(3, 180, 204)*/ yellow)
	val highlighted_border_list = IndexedSeq(border(blue,2), border(blue,2), border(blue,2), border(tan1,2), border(cyan,2))

	val bottom_panel_color_list = IndexedSeq(white, cyan, white, tan1, dodger_blue)

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

class Demineur_Help_Frame extends Frame {
	title = "Help"
	contents = new Label("Come on, you know the rules of this game. ;)")
	visible = true
}

class Demineur_About_Frame extends Frame{
	title = "About"
	contents = new Label("Graphical Interface by G.Hocquet and T.Dupriez")
	visible = true
}

object Demineur extends Game{
	val title = "Démineur"

	val square_size_x = 30
	val square_size_y = 30
	var game_beginning_time: Date = null
	//var in_game = false héritée de Game

	//##Game parameters##
	var numeric_game_parameters_def_list = IndexedSeq(("Width", 0, 4, 25), ("Height", 0, 4, 25), ("Mines", 0, 10, 10))
	var string_game_parameters_def_list = IndexedSeq(("Difficulty", "Easy", IndexedSeq("Easy", "Medium", "Hard", "Tricky")), ("Colour Mode", "Classic", IndexedSeq("Classic", "Creepy-Glauque", "RVB", "Automne", "Ocean")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def nb_of_bombs = numeric_game_parameters_def_list(2)._2 //Ces deux fonctions réalisent un alias du champd valeur du 3ième paramètre numérique du Démineur
	def color_parameter = string_game_parameters_def_list(1)._2
		
	//Conservé pour futurs références mais inutile dans le démineur
	/*def nb_of_bombs = game_parameter_1 //Ces deux fonctions font de nb_of_bombs un alias de la variable game_parameter_1
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval }*/

	type Game_Label_Class = Demineur_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	def about_frame_factory () = { new Demineur_About_Frame }
	def help_frame_factory () = { new Demineur_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(9, 9, 10),IndexedSeq("Easy", "Classic")),
		Game_Mode(IndexedSeq(16, 16, 40),IndexedSeq("Medium", "Classic")),
		Game_Mode(IndexedSeq(16, 16, 99),IndexedSeq("Hard", "Classic"))	
	)
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) ={ //form_nb_fields_result(0) = nb_of_cols, form_nb_fields_result(1) = nb_of_rows, form_nb_fields_result(2) = nb_of_bombs
		//val return_value = form_nb_fields_result(1) * form_nb_fields_result(0) > 9 && form_nb_fields_result(2) + 9 <= form_nb_fields_result(1) * form_nb_fields_result(0)
		var return_value = "OK"
		if (form_nb_fields_result(1) * form_nb_fields_result(0) <= 9) 
			return_value = "Grille trop petite"
		if (form_nb_fields_result(2) + 9 > form_nb_fields_result(1) * form_nb_fields_result(0))
			return_value = "Pas assez de place dans la grille pour les mines"
		return_value
				
	}	

	def game_starter () = {
		Demineur.maj_nb_flag(0)
		game_frame_content.bottom_panel.background = DGE.bottom_panel_color_list(DGE.no_color_mode)
	}
	def game_action_restart() : Unit = {
		if (Demineur.game_frame_content != null) {
			val grid_contents = Demineur.game_frame_content.grid.get_contents
			grid_contents.foreach(label => label.init())

			Demineur.nb_discovered_square = 0
			Demineur.nb_flagged_square = 0
			Demineur.maj_nb_flag(0)
		}
	}
	//Définit ce qui se passe en cas de victoire du joueur -> voir Game
	override def win() = {
		super.win()		
	}
	//Définit ce qui se passe en cas de défaite du joueur -> voir Game
	override def lose() = {
		super.lose()
	}

	//##Demineur Variables## // Variables internes au Démineur
	var nb_discovered_square = 0
	var nb_flagged_square = 0

	//##Demineur Functions## //Fonctions internes au Démineur

	//Incremente le nombres de cases découvertes et déclenche éventuellement la victoire
	def increment_nb_discovered_square() = {
		nb_discovered_square += 1
		if (nb_discovered_square + nb_of_bombs == nb_of_rows * nb_of_cols)
			win()
	}

	//A un numéro de case, associe la liste des numéros des cases adjacentes(en faisant attention aux bords de la grille)
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

	//Met à jour le nombre de cases marquées par un drapeau en accord avec son argument. Met à jour le label du nombre de drapeaux de la fenetre (label_1)
	def maj_nb_flag(n : Int /*normalement 1, -1 ou 0*/) = {
		n match {
			case 1 => nb_flagged_square = nb_flagged_square + n 
			case -1 => nb_flagged_square = nb_flagged_square + n
			case 0 => nb_flagged_square = nb_flagged_square + n
			case _ => println("anormal: la fonction maj_nb_flag de l'objet Demineur a été appelée avec un argument différent de 1, -1 ou 0:" + n)
		}
		val label_1 = game_frame_content.label_1
		label_1.text = "Mines : " + nb_flagged_square.toString + " / " + nb_of_bombs.toString
		if (nb_flagged_square > nb_of_bombs)
			label_1.foreground = DGE.red
		else
			label_1.foreground = DGE.black
	}

	//Est appelée lors du premier clic sur un label.
	//Place les bombes parmi les labels de la grille (autre que le label cliqué et ses 8 voisins).
	//Indique ensuite à chaque label (autre que ceux contenant une bombe) le nombre de ses voisins contenant une bombe -> label.value
	def place_bombs(n_origin_label : Int) = {
		val grid = game_frame_content.grid
		var bombs_left = nb_of_bombs
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
	//Un label qui se découvre avec une valeur égale à 0 (ie aucun de ses voisins ne contient de bombes) appelle cette fonction pour que ses voisins se découvrent
	def spread(numero : Int) = {
		val grid_content = game_frame_content.grid.get_contents
		var voisins_list = neighbour(numero)
		voisins_list.foreach(numero => grid_content(numero).discover())
		
	}
}


object Main {
	def main(args: Array[String]) {
		val ui = new UI(Demineur)
		ui.visible = true
	}
}
