import cats._

package object sphynx {

  type MonadProgram[Alg[_[_]], A] = Program[Alg, Monad, A]
  type ApplicativeProgram[Alg[_[_]], A] = Program[Alg, Applicative, A]
  type ApplyProgram[Alg[_[_]], A] = Program[Alg, Apply, A]

}
