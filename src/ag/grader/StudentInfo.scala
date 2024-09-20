package ag.grader

import upickle.default.ReadWriter

case class StudentInfo(csid: CSID, owner: CSID, real_name: String, public_key: String) derives ReadWriter {

}
