import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

abstract class Grid_Label extends Label{
	var x = 0
	var y = 0
	var numero = 0
	var state: String

	var custom_painting: ((Graphics2D,Label) => Unit) = ((g:Graphics2D, l:Label) => ())
	override def paint(g: Graphics2D)={
		super.paint(g)
		custom_painting(g,this)
	}

	listenTo(mouse.moves, mouse.clicks)
	//Ces fonctions sont à overrider par les labels des jeux pour définir leurs réactions face à différents évènements de souris
	def mouse_enter_reaction () ={	//Lorsque la souris entre dans la zone du label
	}
	def mouse_exit_reaction () ={	//Lorsque la souris quitte la zone du label
	}
	def mouse_leftclic_reaction () ={	//Lorsque le label est cliqué avec le clic gauche
	}
	def mouse_middleclic_reaction () ={	//Lorsque le label est cliqué avec le clic central
	}
	def mouse_rightclic_reaction () ={	//Lorsque le label est cliqué avec le clic droit
	}
	reactions += {
		case e: MouseEntered =>
			mouse_enter_reaction()
		case e: MouseExited =>
			mouse_exit_reaction()
		case e: MouseClicked =>
			e.peer.getButton match {
				case java.awt.event.MouseEvent.BUTTON1 =>
					mouse_leftclic_reaction()
				case java.awt.event.MouseEvent.BUTTON2 =>
					mouse_middleclic_reaction()
				case java.awt.event.MouseEvent.BUTTON3 =>
					mouse_rightclic_reaction()
			}					
	}
}

//Est ce qu'on pourrait se défaire du paramètrage de Grid avec Game_Label_Class en allant chercher le type Game_Label_Class de game ??

//Crée un GridPanel d'une taille correspondant aux paramètres du jeu, puis le remplit avec des labels de la classe passée en argument.
// Fournit aussi 3 fonctions pour accéder aux labels de la grille ainsi créée
class Grid[Game_Label_Class <: Grid_Label] (game: Game) extends GridPanel(game.numeric_game_parameters_def_list(1)._2, game.numeric_game_parameters_def_list(0)._2) /*GridPanel prend le nb de lignes puis le nb de colonnes de la grille*/{
	val nb_of_cols = game.numeric_game_parameters_def_list(0)._2
	val nb_of_rows = game.numeric_game_parameters_def_list(1)._2
	//Remplir la grille d'objets de la classe Game_Label_Class
	for (cy<-1 to nb_of_rows) {
		for (cx<- 1 to nb_of_cols) {
			val label = game.glb_factory()
			label.x = cx-1; label.y = cy-1; label.numero = (cy-1)*nb_of_cols +(cx-1);
			//Les labels sont numérotés de gauche à droite puis de haut en bas. La numérotation commence à 0 en haut à gauche de la grille
			contents += {label}
		}
	}
	minimumSize = new Dimension(game.square_size_x * game.numeric_game_parameters_def_list(0)._2, game.square_size_y * game.numeric_game_parameters_def_list(1)._2 )

	/*//Test
	revalidate()
	repaint()*/
	
	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access_xy(x: Int, y: Int) ={
		contents(y*nb_of_cols + x).asInstanceOf[Game_Label_Class]
	}
	//Renvoit le label de numéro n
	def access_n(n: Int) ={
		contents(n).asInstanceOf[Game_Label_Class]
	}
	//Renvoit la liste des labels de la grille
	def get_contents() = {
		contents.map((x) => x.asInstanceOf[Game_Label_Class])
	}
}