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
			for (i <- 0 to result.length -1) { // Sert à vérifier que tout les champs du formulaire ont été remplis avant le clic sur le bouton "fini"
				if (number_fields_list(i).text.length <= 0){
					nonempty_condition = false
					number_fields_list(i).text = ((fields_bounds_list(i)._1 + fields_bounds_list(i)._2)/2).toString
				}
			}
			if (nonempty_condition == true) {
				result = number_fields_list map (number_field => number_field.text.toInt)
				var bound_condition = true
				for (i <- 0 to result.length - 1 ) { // Sert à vérifier que les valeurs entrées dans les champs du formulaires sont bien dans les limites définies par fields_bounds_list
													// Si ca n'est pas le cas, réinitialise la valeur du champ incorrect avec la moyenne de ses bornes inf et sup
					if (!((fields_bounds_list(i)._1 <= result(i) && result(i) <= fields_bounds_list(i)._2)
						|| fields_bounds_list(i)._1 == fields_bounds_list(i)._2)) {
						bound_condition = false
						number_fields_list(i).text = ((fields_bounds_list(i)._1 + fields_bounds_list(i)._2)/2).toString
					}
				}
	
				if (bound_condition) {	// Le formulaire a été correctement remplis, on "supprime la fenetre du formulaire et 
										// la fonction qui a appelée le formulaire peut en récupérer les résultats dans l'IndexedSeq "result"
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
//les élements de nb_fields_def_list sont de la forme: (nom_du_champ_numérique, bornes_inf_du_résultat_attendu, borne_sup_du_résultat_attendu) (si borne_inf et borne_sup sont égales, il n'y a pas de contraintes)
//les éléments de comboxes_def_list sont de la forme: (nom_de_la_combobox, IndexedSeq_des_chaines_de_charactères_de_la_combobox)
class Form(titre : String, nb_fields_def_list: IndexedSeq[(String,Int,Int)] /*number_fields_names_list : IndexedSeq[String], number_fields_bounds_list : IndexedSeq[(Int,Int)]*/, comboboxes_def_list: IndexedSeq[(String, IndexedSeq[String])]) extends Dialog {
	val this_form = this
	var nb_fields_results: IndexedSeq[Int] = null
	var comboboxes_results: IndexedSeq[String] = null
	var nb_fields_list: IndexedSeq[Number_Field] = null
	var comboboxes_list = IndexedSeq(new ComboBox(Seq("bidon1", "bidon2")))
	var nb_fields_panel = new GridPanel(1,1)
	var comboboxes_panel = new GridPanel(1,1)
	val no_nb_fields = (nb_fields_def_list == null)
	val no_comboboxes = (comboboxes_def_list == null)
	var form_accepted : Boolean = false
	title = titre
	modal = true
	//Number Fields
	if (!no_nb_fields) {
		nb_fields_results = nb_fields_def_list map (nb_field_def => nb_field_def._2)
		nb_fields_list = nb_fields_def_list map (nb_field_def =>
			nb_field_def match {
				case (nb_field_name, inf_bound, sup_bound) =>
				new Number_Field(((inf_bound + sup_bound)/2).toString)
			}
		)
		nb_fields_panel = new GridPanel(nb_fields_def_list.length + 1, 2){
			for (i <- 0 until nb_fields_def_list.length) {
				nb_fields_def_list(i) match {
					case (field_name, inf_bound, sup_bound) =>
						var bounds_string = " (" + inf_bound + "/" + sup_bound + ")"
						if (inf_bound == sup_bound) { bounds_string = ""}
						contents += new Label(field_name + bounds_string + " : ")
						contents += nb_fields_list(i)						
				}

			}
			contents += new Label("")
		}


		/*var nb_fields_list = nb_fields_bounds_list map (couple =>
			couple match {
				case (min_value,max_value) => new Number_Field(((max_value + min_value)/2).toString)
			})
		val nb_fields_panel = new GridPanel(nb_fields_names_list.length + 1, 2) {
			for (i <- 0 until nb_fields_names_list.length) {
				var bounds_string = "  (" + nb_fields_bounds_list(i)._1 + "/" + nb_fields_bounds_list(i)._2 + ")"
				if (nb_fields_bounds_list(i)._1 == nb_fields_bounds_list(i)._2) { bounds_string = ""}
				contents += new Label(nb_fields_names_list(i) + bounds_string + " : ")
				contents += nb_fields_list(i)
			}
			contents += new Label("")
		}*/
	}
	//Comboboxes
	if (!no_comboboxes){
		comboboxes_results = comboboxes_def_list map (combobox => "")
	
		comboboxes_list = comboboxes_def_list map (combobox_def => 
			combobox_def match {
				case	(combobox_name, combobox_item_seq) =>
					new ComboBox(combobox_item_seq)
			}
		)		
		comboboxes_panel = new GridPanel(comboboxes_def_list.length + 1, 2){

			for (i <- 0 until comboboxes_def_list.length) {
				contents += new Label(comboboxes_def_list(i)._1)
				contents += comboboxes_list(i)
			}
			contents += new Label("")
		}
	}
	//Contenu final
	val form_panel = new BoxPanel(Orientation.Vertical){
		contents += nb_fields_panel
		contents += comboboxes_panel
		contents += new Button("") {
			action = Action("Fini")(submit)
		}
		def submit = {
			//Construction de nb_fields_results (IndexedSeq contenant les résultats des champs numériques)
			if(!no_nb_fields) {
				var nonempty_condition = true
				for (i <- 0 to nb_fields_results.length -1) { // Sert à vérifier que tout les champs du formulaire ont été remplis avant le clic sur le bouton "fini"
					if (nb_fields_list(i).text.length <= 0){
						nonempty_condition = false
						nb_fields_def_list(i) match {
							case (field_name, inf_bound, sup_bound) =>
								nb_fields_list(i).text = ((inf_bound + sup_bound)/2).toString
						}

					}
				}
				if (nonempty_condition == true) {
					nb_fields_results = nb_fields_list map (nb_field => nb_field.text.toInt)
					var bound_condition = true
					for (i <- 0 to nb_fields_results.length - 1 ) { // Sert à vérifier que les valeurs entrées dans les champs du formulaires sont bien dans les limites définies par nb_fields_bounds_list
														// Si ca n'est pas le cas, réinitialise la valeur du champ incorrect avec la moyenne de ses bornes inf et sup
						nb_fields_def_list(i) match{
							case (field_name, inf_bound, sup_bound) =>
								if (!((inf_bound <= nb_fields_results(i) && nb_fields_results(i) <= sup_bound)
								|| inf_bound == sup_bound)) {
									bound_condition = false
									nb_fields_list(i).text = ((inf_bound + sup_bound)/2).toString
								}							
						}

					}
		
					if (bound_condition) {	// Le formulaire a été correctement remplis, on "supprime la fenetre du formulaire et 
											// la fonction qui a appelée le formulaire peut en récupérer les résultats dans l'IndexedSeq "nb_fields_results"
						form_accepted = true
						this_form.visible = false
					}
					else {
						println("Les réponses aux formulaires ne sont pas dans les bornes définies")
					
					}
				}
				else {
					println("Certains des champs du formulaire sont vides")
				}
			}
			//Construction de comboboxes_results (IndexedSeq contenant les résultats des comboboxes)
			if (!no_comboboxes) {
				comboboxes_results = comboboxes_list map (combobox => combobox.item)
			}
		}
	}
	contents = form_panel
	visible = true
}