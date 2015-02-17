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

trait Label_States extends Colors with Label_Borders with Game_Data{
	//state0=Label Unexplored//state1=Label Marqued//state2=Label Explored//
	val s_size_x = 			Array(square_size_x,			square_size_x,			square_size_x)
	val s_size_y = 			Array(square_size_y,			square_size_y,			square_size_y)
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

	/*def factory () = {
		val newlabel = new Grid_Label
		change_To_State(newlabel,0)
		newlabel
	}*/
	change_To_State(this,state)

	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...

	//def factory()
}
/* // Grid_Label était un abstract trait
class Demineur_Label extends Grid_Label {
		font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
		def factory() = {
			new Demineur_Label
		}
}*/


/*Init_Label_Class doit etre un sous-type de la classe Grid_Label*/
class Grid (row_size: Int, col_size: Int) extends GridPanel(row_size,col_size) {
	val Marge = 10

	/*
	def factory() : Init_Label_Class = {
		//manifest[Init_Label_Class].erasure.newInstance().asInstanceOf[Init_Label_Class]
		mymanifest.erasure.newInstance().asInstanceOf[Init_Label_Class]
	}
	*/

	//Remplir la grille d'objet de la classe Grid_Label
	for (cx<-1 to row_size) {
		for (cy<- 1 to col_size) {
			//val new_label = manifest[Grid_Label].erasure.newInstance().asInstanceOf[Init_Label_Class]
			//val new_label = factory()
			contents += {new Grid_Label{x=cx; y=cy; numero=(cy-1)*row_size + (x-1)} }
			//val new_label = Init_Label_Class.factory()
			//new_label.x = cx
			//contents += {new_label}
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
trait Game_Data {
	var square_size_x = 50
	var square_size_y = 50
	var square_dimension = new Dimension(square_size_x,square_size_y)
}

object Game extends Game_Data{
	val title = "Démineur"
	var nb_discovered_square = 0
	var nb_marqued_square = 0
	var nb_of_bombs = 0
	var in_game = false


	//Example of what should be here:
	/*def neighbour(n : Int) : List[Int] = {
		
	}*/

	/*def game_starter(frame: Frame,nb_of_rows: Int, nb_of_cols: Int, nb_of_bombs: Int) = {
		val grid = new Grid(nb_of_rows,nb_of_cols)
		frame.contents = grid

		//val str = grid.contents(3).asInstanceOf[String]
		//str.asInstanceOf[Label].background = new Color(22,9,255)
		//println("de")
	}*/

		/*"AM" -> "Action Maker"*/  //Pour pouvoir l'utiliser comme une action dans des menus alors que gamestarter prend des arguments
	class AM_Game_Starter(frame: Frame,nb_of_rows: Int, nb_of_cols: Int, nb_of_bombs: Int) {
		def action () = {
			var game_beginning_time = new Date()

			val grid = new Grid(nb_of_rows,nb_of_cols)
			frame.contents = grid

			val timer_label = new Timer_Label(game_beginning_time)
			timer_label.preferredSize = new Dimension(100,50)

			val flow_panel = new FlowPanel() {
				//Labels
				contents += timer_label

			}

			val border_panel = new BorderPanel {
				layout(grid) = North
				layout(flow_panel) = South
			}

			frame.contents = border_panel
		}
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
	/*class Action_Listener extends ActionListener {
		def actionPerformed(e: ActionEvent) {
			var minutes  = ((new Date).getTime() - time_origin.getTime()) / 60000 % 60
			var secondes = ((new Date).getTime() - time_origin.getTime()) / 1000 % 60
			var str = if (minutes < 10) "0" else ""
			str = str + minutes.toString + ":"
			str = if (secondes < 10) str + "0" else str
			str = str + secondes.toString
		}
	}*/
	
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

	val timer = new javax.swing.Timer(1, timer_listener)

	timer.start()
}

/*
class Timer extends javax.swing.Timer(1000, Swing.ActionListener(e => action_to_perform)) {
	//actionPerformed = Swing.ActionListener(e => {})
	def action_to_perform () = {

	}
	this.start
} 

class Timer_Action_Listener extends ActionListener {

}*/

/*// Ce serait cool de faire un truc plus générique ici
	/*"MI" -> "MenuItem"*/
class MI_Game_Starter (frame: Frame,nb_of_rows: Int, nb_of_cols: Int, nb_of_bombs: Int) extends MenuItem("") {
	val title = "Grille "+ nb_of_rows + " * " + nb_of_cols + ", " + nb_of_bombs + " bombes"
	/*action_function*/
	def action_f = {
		Game.game_starter(frame,nb_of_rows,nb_of_cols,nb_of_bombs)
	}
	action = Action(title)(action_f)
	
}
	/*"PMI" -> "Parametrized MenuItem" Prend en argument une fonction qui renvoit Unit et ses arguments et*/
/*class PMI () extends MenuItem{

}*/
*/

/*
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
*/

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
                    //contents += new MenuItem(""){action = Action_Manager.am_game_starter("Grille 9*9, 10 bombes",thisui,9,9,10)}
                    //contents += new MenuItem(""){action = Action_Manager.am_game_starter("Grille 16*16, 40 bombes",thisui,16,16,40)}
                    //contents += new MenuItem(""){action = Action_Manager.am_game_starter("Grille 16*16, 99 bombes",thisui,16,16,99)}
                    //contents += new MI_Game_Starter(thisui,6,10,5)

                    val am1 = new Game.AM_Game_Starter(thisui,9,9,10)
					contents += new MenuItem(""){action = Action("Grille 9*9, 10 bombes")(am1.action)}
                    val am2 = new Game.AM_Game_Starter(thisui,5,5,7)
                    contents += new MenuItem(""){action = Action("Grille 5*5, 7 bombes")(am2.action)}
			/*contents += new GrilleMode(t)*/
                }
                contents += new Menu("About") {
                	//contents +=new MenuItem(""){action = Action_Manager.a_about}
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
