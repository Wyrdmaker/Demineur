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




class Grid[Game_Label_Class <: Grid_Label] (row_size: Int, col_size: Int, factory : Unit => Game_Label_Class) extends GridPanel(row_size,col_size) {
	//val Marge = 10



	//Remplir la grille d'objet de la classe Grid_Label
	for (cx<-1 to row_size) {
		for (cy<- 1 to col_size) {
			val label = factory()
			label.x = cx; label.y = cy; label.numero = (cy-1)*row_size +(cx-1);
			contents += {label}
		}
	}

	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access_xy(x: Int, y: Int) ={
		contents(y*row_size + x).asInstanceOf[Game_Label_Class]
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