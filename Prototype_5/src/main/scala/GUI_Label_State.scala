import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

//Les états de label sont des sets de paramètres graphiques et contienent une fonction permettant de changer les paramètres graphiques d'un 
//	label pour qu'ils s'accordent à ceux de l'état
abstract class Label_State[Game_Label_Class <: Grid_Label] {
	val state_name: String

	//val size_x: Int
	//val size_y: Int
	val label_border: javax.swing.border.Border
	val opaque: Boolean
	val background: Color
	val foreground: Color
	val text: String
	var custom_painting: ((Graphics2D,Label) => Unit) = ((g:Graphics2D, l:Label) => ())

	def change_to_state(game_label: Game_Label_Class) = {
		game_label.state = state_name
		//game_label.preferredSize = new Dimension(size_x,size_y)
		game_label.border = label_border
		game_label.opaque = opaque
		game_label.background = background
		game_label.foreground = foreground
		game_label.text = text
		game_label.custom_painting = custom_painting
	}
}