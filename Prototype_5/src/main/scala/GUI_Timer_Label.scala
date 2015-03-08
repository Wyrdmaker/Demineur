import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

//DÉFAUT: le thread utilisé par le timer ne s'arrète pas, m^eme après avoir fermé le jeu (écrire un println("tick") dans la fonction actionPerformed)

//Idée: écrire un truc pour qu'on puisse dire à une action de s'éxécuter dans n secondes (lance un timer avec timeout puis execute l'action)

//La classe Timer_Label fournit des labels chronomètres dotés de 3 fonctions: 
//	restart -> change l'origine temporelle du chronomètre
//	start -> lance le chronomètre
//	stop -> arrete le chronomètre
class Timer_Label (time_origin_arg : Date) extends Label{
	val this_timer_label = this
	var time_origin = time_origin_arg
	var minutes = ((new Date).getTime() - time_origin.getTime()) / 60000 % 60
	var secondes = ((new Date).getTime() - time_origin.getTime()) / 1000 % 60
	text = "00:00"

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
}