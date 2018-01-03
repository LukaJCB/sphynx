import cats._

package object sphynx {

  type ApplicativeProgram[Alg[_[_]], A] = Program[Alg, Applicative, A]
  type ApplyProgram[Alg[_[_]], A] = Program[Alg, Apply, A]

}
