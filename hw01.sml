fun intToString(x : int) : string = Int.toString x

fun double(n : int) : int = 2 * n
val y : string = "th"
fun suffix(x : int) = intToString(x) ^ y 



val z : string = suffix 4
