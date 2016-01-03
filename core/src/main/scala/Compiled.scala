package com.todesking.fast_function_composer

abstract class Compiled[A, B](val src: FastComposable[A, B]) extends Function1[A, B]
