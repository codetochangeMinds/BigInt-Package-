signature BigInt =
  sig
    eqtype bigint

    val fromInt :	int    -> bigint
    val toInt :		bigint -> int			(* [Overflow] *)
    val toString :	bigint -> string

    val < :		bigint * bigint -> bool
    val <= :		bigint * bigint -> bool
    val isNegative :	bigint          -> bool
    val isEven :	bigint          -> bool

    val ~ :		bigint          -> bigint
    val + :		bigint * bigint -> bigint
    val - :		bigint * bigint -> bigint
    val * :		bigint * bigint -> bigint
    val div :		bigint * bigint -> bigint	(* [Div] *)
    val mod :		bigint * bigint -> bigint	(* [Div] *)
    val abs :		bigint          -> bigint
    val exp :		bigint * bigint -> bigint	(* [Domain] *)
  end
