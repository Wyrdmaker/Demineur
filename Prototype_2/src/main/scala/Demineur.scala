import scala.swing._
import scala.swing.event._
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

class Grid [Init_Label_Class <: Label](row_size: Int, col_size: Int, fac: () => Init_Label_Class) extends GridPanel(row_size,col_size) {
	//val label_Matrix = 
	//val grid = new GridPanel(row_size,col_size)
	//val l = Label()
	//val ll = Label_Test_1()
	var label_Matrix



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
*/
	
}


trait Grid_Panel_Attributes_1 {
	val Marge = 15

	def Apply_Attributes_1(): Unit ={

	}
}

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

class UI extends MainFrame {
	title = "Démineur"
	preferredSize = new Dimension(300,300)
	contents = new Label("Welcome ! ;)")
	//contents = new FlowPanel(new Label_Test_1, new Label_Test_2 )


}


object GuiProgram {
	def main(args: Array[String]) {
		val ui = new UI
		ui.visible = true
	}
}
