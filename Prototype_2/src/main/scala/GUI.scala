import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
//import javax.swing.{ImageIcon, Icon}

//Une Bonne Idée mais ça demande du boulot
//Demineur extends Game
/*
class GUI(game: Game) {
	//new Array[game.Game_Label]

}
*/
abstract class Game{
	type Game_Label_Class <: Grid_Label
	def glb_factory () : Game_Label_Class
	val square_size_x: Int
	val square_size_y: Int
	var nb_of_rows: Int
	var nb_of_cols: Int
	var game_beginning_time: Date
}



class Grid[Game_Label_Class <: Grid_Label] (nb_of_cols: Int, nb_of_rows: Int, factory : Unit => Game_Label_Class) extends GridPanel(nb_of_rows,nb_of_cols) /*GridPanel prend le nb de lignes puis le nb de colonnes*/{
	//val Marge = 10



	//Remplir la grille d'objet de la classe Grid_Label
	for (cy<-1 to nb_of_rows) {
		for (cx<- 1 to nb_of_cols) {
			val label = factory()
			label.x = cx-1; label.y = cy-1; label.numero = (cy-1)*nb_of_cols +(cx-1);
			contents += {label}
		}
	}

	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access_xy(x: Int, y: Int) ={
		contents(y*nb_of_cols + x).asInstanceOf[Game_Label_Class]
	}
	def access_n(n: Int) ={
		contents(n).asInstanceOf[Game_Label_Class]
	}
	def get_contents() = {
		contents.map((x) => x.asInstanceOf[Game_Label_Class])
	}
}

class Grid_Label extends Label with Colors with Label_Borders with Label_States{
	var x = 0
	var y = 0
	var numero = 0
	var state = 0

	override def change_to_state (label: Label,no_state : Int): Unit ={
		state = no_state
		super.change_to_state(this,no_state)
	}
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