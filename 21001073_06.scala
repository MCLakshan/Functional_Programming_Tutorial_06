import scala.io.StdIn

object tute_06 extends App{

//The Encryption Letter Function
def Encrypt_letter(text:String, shift_num:Int):Char =  text(0) match{
	case ' ' => ' '
	case _ =>{
		if ((text(0).toInt + shift_num) > 'Z'.toInt)
			('A'.toInt + shift_num -1 + (text(0).toInt - 'Z'.toInt)).toChar
		else
			(text(0).toInt + shift_num).toChar
	}
}


//The Encryption String Function
def Encrypt_string(s:String, shift_num:Int):String = s.length() match {
	case 0 => ""
	case _ => Encrypt_letter(s, shift_num).toString + Encrypt_string(s.substring(1), shift_num)
}


//The Decryption Letter Function
def Decrypt_letter(text:String, shift_num:Int):Char =  text(0) match{
	case ' ' => ' '
	case _ =>{
		if ((text(0).toInt - shift_num) < 'A'.toInt)
			('Z'.toInt - shift_num + 1 + (text(0).toInt - 'A'.toInt)).toChar
		else
			(text(0).toInt - shift_num).toChar
	}
}


//The Decryption String Function
def Decrypt_string(s:String, shift_num:Int):String = s.length() match {
	case 0 => ""
	case _ => Decrypt_letter(s, shift_num).toString + Decrypt_string(s.substring(1), shift_num)
}



//The Cipher function...
def Cipher() = {
	println("________MEMU________")
	println("Options:")
	println("1 - Encrypt a string")
	println("2 - Decrypt a string")
	print("select>")
	var option:Int = StdIn.readInt();

	option match{
		case 1 => {
			print("\nEnter the string to be Encrypted : ")
			var str:String = StdIn.readLine().toUpperCase()
			print("Enter the number of shifts (key) : ")
			var shifts:Int = StdIn.readInt()
			var Encrypted:String = Encrypt_string(str, shifts)
			println("\tThe Unencrypted string : " + str)
			println("\tThe Encrypted string   : " + Encrypted)
		}
		case 2 => {
			print("\nEnter the string to be Decrypted : ")
			var str:String = StdIn.readLine().toUpperCase()
			print("Enter the number of shifts (key) : ")
			var shifts:Int = StdIn.readInt()
			var Dencrypted:String = Decrypt_string(str, shifts)
			println("\tThe Encrypted string : " + str)
			println("\tThe Dencrypted string   : " + Dencrypted)
		}
	case _ => println("\nInvalid Input!")
	}
}

Cipher()


}

