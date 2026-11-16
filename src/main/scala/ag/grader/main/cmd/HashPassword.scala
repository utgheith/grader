package ag.grader.main.cmd

// Prints a hash of a password, for config.json's "web": {"password_hash"}
object HashPassword {
  def apply(): Unit = {
    def ask(prompt: String): Array[Char] = Option(System.console()) match {
      case Some(console) => console.readPassword(prompt).nn
      case None          =>
        print(prompt)
        scala.io.StdIn.readLine().toCharArray.nn
    }
    val password = ask("password: ")
    if (password.isEmpty) println("empty password")
    else if (!password.sameElements(ask("again: "))) println("they differ")
    else println(ag.web.Password.hash(password))
  }
}
