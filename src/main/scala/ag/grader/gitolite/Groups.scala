package ag.grader.gitolite

import ag.grader.name.CourseName

// gitolite's groups, as repos' readers and writers name them
object Groups {
  def staff(c: CourseName): String = s"@${c.value}_staff"
  val everyone: String = "@all"
}
