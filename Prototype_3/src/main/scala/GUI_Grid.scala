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
}

//Est ce qu'on pourrait se défaire du paramètrage de Grid avec Game_Label_Class en allant chercher le type Game_Label_Class de game ??
class Grid[Game_Label_Class <: Grid_Label] (game: Game) extends GridPanel(game.nb_of_rows, game.nb_of_cols) /*GridPanel prend le nb de lignes puis le nb de colonnes*/{
	val nb_of_cols = game.nb_of_cols
	val nb_of_rows = game.nb_of_rows
	//Remplir la grille d'objets de la classe Game_Label_Class
	for (cy<-1 to nb_of_rows) {
		for (cx<- 1 to nb_of_cols) {
			val label = game.glb_factory()
			label.x = cx-1; label.y = cy-1; label.numero = (cy-1)*nb_of_cols +(cx-1);
			//Les labels sont numérotés de gauche à droite puis de haut en bas. La numérotation commence à 0 en haut à gauche de la grille
			contents += {label}
		}
	}
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