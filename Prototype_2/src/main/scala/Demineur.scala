import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
//import javax.swing.{ImageIcon, Icon}

trait Colors {
	val label_Color_Unexplored = new Color(255,100,0)
	val label_Color_Marqued = new Color(255,200,100)
	val label_Color_Explored = new Color(255,50,50)

	val black = new Color(0,0,0,255)
	val black_Dim = new Color(0,0,0,100)
}

trait Label_Borders extends Colors{
	val black_Border = Swing.LineBorder(black)
	val black_Dim_Border = Swing.LineBorder(black_Dim)
}

trait Label_States extends Colors with Label_Borders{
	//state0=Label Unexplored//state1=Label Marqued//state2=Label Explored//
	val s_size_x = 			Array(50,						50,						50)
	val s_size_y = 			Array(50,						50,						50)
	val s_label_bordure = 	Array(black_Border,				black_Border,			black_Dim_Border)
	val s_opaque = 			Array(true,						true,					true)
	val s_background = 		Array(label_Color_Unexplored,	label_Color_Marqued,	label_Color_Explored)
	val s_foreground = 		Array(black,					black,					/*???*/black/*???*/)

	def change_To_State(label : Label,no_state: Int): Unit ={
		label.preferredSize = new Dimension(s_size_x(no_state),s_size_y(no_state))
		label.border = s_label_bordure(no_state)
		label.opaque = s_opaque(no_state)
		label.background = s_background(no_state)
		label.foreground = s_foreground(no_state)
	}
}

class Grid_Label extends Label with Colors with Label_Borders with Label_States{
	var x = 0
	var y = 0
	var numero = 0
	var state = 0
	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
	/*def factory () = {
		val newlabel = new Grid_Label
		change_To_State(newlabel,0)
		newlabel
	}*/
	change_To_State(this,state)

}

class Grid (row_size: Int, col_size: Int) extends GridPanel(row_size,col_size) {
	val Marge = 10

	//Remplir la grille d'objet de la classe Grid_Label
	for (cx<-1 to row_size) {
		for (cy<- 1 to col_size) {
			contents += {new Grid_Label{x=cx; y=cy; numero=(cy-1)*row_size + (x-1)} }
		}
	}

	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access(x: Int, y: Int) ={
		contents(y*row_size + x)
	}
}

/*
class Grid [Init_Label_Class <: Grid_Label](row_size: Int, col_size: Int, fac: () => Init_Label_Class) extends GridPanel(row_size,col_size) {
	//val label_Matrix = 
	//val grid = new GridPanel(row_size,col_size)
	//val l = Label()
	//val ll = Label_Test_1()

	val l = new Init_Label_Class

	//Remplir la grille d'objet de type Init_Label_Class
	for (i <- 1 to row_size*col_size) {
		//contents += {Init_Label_Class.factory()}
	}

	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access(x: Int, y: Int) ={
		contents(y*row_size + x)
	}
*/


/*
	var label_Matrix = Array(y_Array)
	for (x <- 0 to (row_size - 1)) {
		var y_Array = Array()
		for (y <- 0 to (col_size - 1)) {
			val l = fac()
			y_Array = y_Array :+ l
		}
		label_Matrix = label_Matrix ++ y_Array
	}

	
}
*/
/*
trait Grid_Panel_Attributes_1 {
	val Marge = 15

	def Apply_Attributes_1(): Unit ={

	}
}
*/

/*object Label_Test_1 {
	def apply() = new Label_Test_1()
}*/

//##########
//LES CLASSES DE LABEL DOIVENT ÊTRE DES CASE CLASS
//	elles ont ainsi la méthode apply() = new <NOM_CLASSE> 
//	qui est utilisée par la classe GRID
//
//##########

/*case class Label_Test_1 extends Label with Label_States{

	preferredSize = new Dimension(45,50)

	change_To_State(this,1)
	text = "TEST"
}*/

/*case class Label_Test_2 extends Label with Label_States{
	preferredSize = new Dimension(65,40)

	change_To_State(this,0)
		text = "TEST"
}*/
object Game {
	val title = "Démineur"
	var nb_discovered_square = 0
	var nb_marqued_square = 0
	var nb_of_bombs = 0
	var in_game = false

	//Example of what should be here:
	def neighbour(n : Int) : List[Int] = {
		
	}
}

object Action_Manager {
	/*"am" -> "action maker"*/
	def am_game_starter(title: String,/*arguments de game_starter->*/frame: Frame,nb_of_rows: Int, nb_of_cols: Int, nb_of_bombs: Int) = {
		def game_starter = {
			val grid = new Grid(nb_of_rows,nb_of_cols)
			frame.contents = grid
		}
		val action_to_return = new Action(title){def apply {game_starter}}
		action_to_return
	}
	/*"a" -> "action"*/
	def a_about = {
		val action_to_return = new Action("Mysterious"){
									def apply {
										println("Indeed !")
									}
								}
		action_to_return
	}
}

class UI extends MainFrame with Colors{
	val thisui = this
	title = Game.title
	//preferredSize = new Dimension(300,300)
	//resizable = false
	//val grid = new Grid(3,5)
	//contents = grid
	contents = new Label("Welcome ! ;)"){
		preferredSize = new Dimension(300,300)
	}
	//contents = new FlowPanel(new Label_Test_1, new Label_Test_2 )
	menuBar = new MenuBar {
                contents += new Menu("Game") {
                    contents += new MenuItem(""){action = Action_Manager.am_game_starter("Grille 9*9, 10 bombes",thisui,9,9,10)}
                    contents += new MenuItem(""){action = Action_Manager.am_game_starter("Grille 16*16, 40 bombes",thisui,16,16,40)}
                    contents += new MenuItem(""){action = Action_Manager.am_game_starter("Grille 16*16, 99 bombes",thisui,16,16,99)}

			/*contents += new GrilleMode(t)*/
                }
                contents += new Menu("About") {
                	contents +=new MenuItem(""){action = Action_Manager.a_about}
                }
    }

}


object GuiProgram {
	def main(args: Array[String]) {
		val ui = new UI
		ui.visible = true
	}
}
