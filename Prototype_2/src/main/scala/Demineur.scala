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
	val label_color_unexplored = new Color(255,100,0)
	val label_color_marqued = new Color(255,200,100)
	val label_color_explored = new Color(255,50,50)

	val black = new Color(0,0,0,255)
	val black_dim = new Color(0,0,0,100)

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
	val black_border = Swing.LineBorder(black,1)
	val black_dim_border = Swing.LineBorder(black_dim)
	val blue_border = Swing.LineBorder(blue,1)
}

trait Label_States extends Colors with Label_Borders with Demineur_Parameters{
	//state0=Label Unexplored//state1=Label Marqued//state2=Label Explored//
	val s_size_x = 			Array(square_size_x,			square_size_x,			square_size_x)
	val s_size_y = 			Array(square_size_y,			square_size_y,			square_size_y)
	val s_label_bordure = 	Array(black_border,				black_border,			black_dim_border)
	val s_opaque = 			Array(true,						true,					true)
	val s_background = 		Array(label_color_unexplored,	label_color_marqued,	label_color_explored)
	val s_foreground = 		Array(black,					black,					/*???*/black/*???*/)

	def change_to_state(label : Label,no_state: Int): Unit ={
		label.preferredSize = new Dimension(s_size_x(no_state),s_size_y(no_state))
		label.border = s_label_bordure(no_state)
		label.opaque = s_opaque(no_state)
		label.background = s_background(no_state)
		label.foreground = s_foreground(no_state)
	}
}






class Demineur_Label extends Grid_Label{
	var discovered = false
	var value = "?"
	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
	init()

	def init() : Unit = {
		change_to_state(this,0)
		text = ""
		listenTo(mouse.moves, mouse.clicks)
	}

	reactions += {
        case e : MouseEntered =>
			if (!discovered) 
            	border = blue_border
        case e : MouseExited =>
			if (!discovered)
          		border = black_border
		case e : MouseClicked =>
			//if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1 && !flag())
				//discoverMe()
			//else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3)
				//switch()
	}
	
	def discover() : Unit = {
		if (!discovered) {
			//background = LabelColorActif
			//border = LabelBordure
            discovered = true
			Demineur.increment_nb_discovered_square()
			if (value == "?")
				Demineur.place_bombs(numero)
			change_to_state(this,2)
			value match {
				case "b" =>
					//background = LabelColorDetected
					//foreground = ColorBomb
					text = value
					Demineur.lose()
				case "0" =>
					//foreground = /*ColorNum(text.toInt)*/ 
					text = ""
					Demineur.spread(numero)
				case _   =>
					text = value
					foreground = Demineur.demineur_color_list(text.toInt)
			}
		}
	}
	
}




trait Demineur_Parameters extends Colors{
	val square_size_x = 50
	val square_size_y = 50
	var square_dimension = new Dimension(square_size_x,square_size_y)
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

object Demineur extends Game with Demineur_Parameters{
	type Game_Label_Class = Demineur_Label
	def glb_factory () ={new Game_Label_Class } // "glb" -> "Game_Label_Class"
	val title = "Démineur"
	var nb_discovered_square = 0
	var nb_marqued_square = 0
	var nb_of_bombs = 0
	var in_game = false
	var nb_of_rows = 0
	var nb_of_cols = 0
	var game_beginning_time: Date = null
	var grid:Grid[Game_Label_Class] = null
	var game_frame_content: Game_Frame_Content = null

	def increment_nb_discovered_square() = {
		nb_discovered_square += 1
		if (nb_discovered_square + nb_of_bombs == nb_of_rows * nb_of_cols)
			win()
	}

	/*
	def test() ={
		var label = grid.access_xy(2,3)
		label.value = "coucou"
		label.text = "coucou"
		val lbl_list = grid.get_contents
		lbl_list.foreach( x => {x.value = "Tropique"; x.text=x.value})
	}
	*/

	def neighbour(n : Int) : List[Int] = {
		var lst : List[Int]= List()
		var a = if (n % nb_of_cols == 0) 0 else -1
        var b = if (n % nb_of_cols == nb_of_cols - 1) 0 else 1
        var c = if (n < nb_of_cols) 0 else -1
        var d = if (n >= (nb_of_rows - 1) * nb_of_cols) 0 else 1
        for (i <- a to b) {
            for (j <- c to d) {
                if (0 <= n + j * nb_of_rows + i && n + j * nb_of_rows + i < nb_of_rows * nb_of_cols) {
                    lst ++= List(n + j * nb_of_rows + i) // LOLILOOOL
                }
            }
        }
		return lst	
	}

	def place_bombs(n_origin_label : Int) = {
		val grid = game_frame_content.grid
		
		var bombs_left = nb_of_bombs
		var random_gen = scala.util.Random
		//neighbour(n_origin_label).foreach(n => lstLabel(n_origin_label).value = "#")
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
				neighbour(label.numero).foreach(numero => 
					if (grid_label_list(numero).value == "b")
						{ new_value +=1 }
				)
					label.value = new_value.toString
			}
		)

	
		
		//A supprimer
		/*
		lstLabel.foreach(l =>
			if (l.v != "b") {
				var v = 0
				neighbour(l.num).foreach(n => if (lstLabel(n).v == "b") v += 1)
				l.v = v.toString
			}
		)
		*/
	}

	def win() = {
		val end_label = game_frame_content.end_label
		val grid_content = game_frame_content.grid.get_contents
		in_game = false
                end_label.text = "WIN !"
                end_label.background = new Color(0,255,0)
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))
		
	}

	def lose() = {
		/*inGame = false
		labelFin.text = "GAME OVER !"
		labelFin.background = new Color(255,0,0)
		lstLabel.foreach(x => x.deafTo(x.mouse.moves, x.mouse.clicks))
		*/
	}

	def spread(n : Int) = {
		/*var lst = neighbour(n)
		lst.foreach(n => lstLabel(n).discoverMe())
		*/
	}

	//Example of what should be here:
	/*def neighbour(n : Int) : List[Int] = {
		
	}*/
		/*"AM" -> "Action Maker"*/  //Pour pouvoir l'utiliser comme une action dans des menus alors que gamestarter prend des arguments
	class AM_Game_Starter(frame: Frame,nb_of_rows: Int, nb_of_cols: Int, nb_of_bombs: Int) {
		def action () = {
			
			Demineur.game_beginning_time = new Date()
			Demineur.nb_of_rows = nb_of_rows
			Demineur.nb_of_cols = nb_of_cols
			Demineur.nb_of_bombs = nb_of_bombs

			var game_frame_content = new Game_Frame_Content(Demineur)
			Demineur.game_frame_content = game_frame_content

			frame.contents = game_frame_content.final_content

			//Test
			/*
			Demineur.test()
			*/
		}
	}

}



class Game_Frame_Content (game: Game) {

	val grid = new Grid[Demineur_Label](game.nb_of_rows,game.nb_of_cols,unit => new Demineur_Label )

	val end_label = new Label()
	end_label.preferredSize = new Dimension(game.nb_of_rows * game.square_size_x / 3,30)

	val bomb_nb_label = new Label()
	bomb_nb_label.preferredSize = new Dimension(game.nb_of_rows * game.square_size_x / 3,30)

	val timer_label = new Timer_Label(game.game_beginning_time)
	timer_label.preferredSize = new Dimension(game.nb_of_rows * game.square_size_x / 3,30)

	val bottom_panel = new FlowPanel() {
		//Labels
		contents += bomb_nb_label
		contents += end_label
		contents += timer_label

	}

	val border_panel = new BorderPanel {
		layout(grid) = North
		layout(bottom_panel) = South
	}

	val final_content = border_panel

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

	val timer = new javax.swing.Timer(1000, timer_listener)

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
	title = Demineur.title
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

                    val am1 = new Demineur.AM_Game_Starter(thisui,9,9,10)
					contents += new MenuItem(""){action = Action("Grille 9*9, 10 bombes")(am1.action)}
                    val am2 = new Demineur.AM_Game_Starter(thisui,5,5,7)
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
