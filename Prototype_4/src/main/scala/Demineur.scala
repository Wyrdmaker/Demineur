import scala.swing._
import scala.swing.event._
//import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
//import java.awt.event.{ActionEvent, ActionListener}

//import javax.swing.{ImageIcon, Icon}

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

object Demineur extends Game with Demineur_Graphical_Elements{
	val title = "Démineur"

	val square_size_x = 35
	val square_size_y = 35
	var game_beginning_time: Date = null
	//var in_game = false héritée de Game

	//##Game parameters##
	var nb_of_rows = 0
	var nb_of_cols = 0
	def nb_of_bombs = game_parameter_1.toInt //Ces deux fonctions réalisent un alias de game_parameter_1: number_of_bombs qui sera utilisé comme un entier par le code du demineur
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval.toString }
	val game_parameter_1_name = "Mines"
	//var game_parameter_2 = "" héritée de Game
	val game_parameter_2_name = ""


	type Game_Label_Class = Demineur_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	def about_frame_factory () = { new Demineur_About_Frame }
	def help_frame_factory () = { new Demineur_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game

	val game_difficulty_mode_list = IndexedSeq(
		Difficulty_Mode(9, 9, "10", "", "Easy"),
		Difficulty_Mode(16, 16, "40", "", "Medium"),
		Difficulty_Mode(16, 16, "99", "", "Hard")
	)
	val custom_game_parameters_bounds = IndexedSeq((4,25),(4,25),(10,10))
	def custom_game_parameters_conditions (form_result: IndexedSeq[Int]) ={ //form_result(0) = nb_of_cols, form_result(1) = nb_of_rows, form_result(2) = nb_of_bombs
	 	val return_value = form_result(1) * form_result(0) > 9 && form_result(2) + 9 <= form_result(1) * form_result(0)
		return_value
 				
	}	
	def game_custom_mode () = {		
		var custom_grid_form = new Number_Form(
			"Grille Perso",
			IndexedSeq("x", "y",  "mines"),
			IndexedSeq((4,25), (4,25), (10,10))
		)
		val form_result = custom_grid_form.result
		val asked_nb_of_cols = form_result(0)
		val asked_nb_of_rows = form_result(1)
		val asked_nb_of_bombs = form_result(2)
		if (	custom_grid_form.form_accepted 
			&& 	asked_nb_of_cols * asked_nb_of_rows > 9 
			&& 	asked_nb_of_bombs + 9 <= asked_nb_of_cols * asked_nb_of_rows) {
			new Difficulty_Mode(asked_nb_of_cols, asked_nb_of_rows, asked_nb_of_bombs.toString,"", "Custom")
		}
		else {
			println("Les réponses au formulaire ne permettent pas de créer une grille convenable")
			throw new Custom_Mode_Exception("formulaire pour mode custom mal rempli")

		}		
	}


	def game_starter () = {
		Demineur.maj_nb_flag(0)
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

	//##Demineur Variables##
	var nb_discovered_square = 0
	var nb_flagged_square = 0

	//##Demineur Functions##

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
			label_1.foreground = red
		else
			label_1.foreground = black
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
