package sphynx.syntax

trait AllSyntax extends OptimizerSyntax
  with MonadSyntax

object all extends AllSyntax
