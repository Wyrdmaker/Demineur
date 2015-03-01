import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

class Number_Field(init_string : String) extends TextField(init_string) {
	listenTo(keys)
	reactions += {
		case e : KeyTyped =>
			if (!e.char.isDigit)
				e.consume
	}
}
//La classe Number Form permet de demander au joueur des renseignements chiffrés
//Les noms des champs doivent etre fournis sous forme de IndexedSeq -> fields_names_list
//Les couples d'Int de fields_bounds_list représentent le min et le max que l'utilisateur peut rentrer dans le formulaire pour chaque champs
//( un couple de la forme (n,n) avec n un entier signifie pas de restriction )
//La valeur par défaut des champs sera la moyenne du min et du max du champs en question
class Number_Form(titre : String, fields_names_list : IndexedSeq[String], fields_bounds_list : IndexedSeq[(Int,Int)]) extends Dialog {
	var result: IndexedSeq[Int] = fields_bounds_list map (couple => couple._1)
	var form_accepted : Boolean = false
	if (fields_names_list.length == fields_bounds_list.length) {
		title = titre
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
				action = Action("Fini")(submit)
			}
		}

		def submit = {
			var nonempty_condition = true
			for (i <- 0 to result.length -1) {
				if (number_fields_list(i).text.length <= 0){
					nonempty_condition = false
					number_fields_list(i).text = ((fields_bounds_list(i)._1 + fields_bounds_list(i)._2)/2).toString
				}
			}
			if (nonempty_condition == true) {
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
					form_accepted = true
					visible = false
				}
				else {
					println("Les réponses aux formulaires ne sont pas dans les bornes définies")
				
				}
			}
			else {
				println("Certains des champs du formulaire sont vides")
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