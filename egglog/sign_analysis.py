# mypy: disable-error-code="empty-body"

from __future__ import annotations

from typing import ClassVar

from egglog import *

egraph = EGraph()


@egraph.class_
class Exp(Expr):
    def __init__(self, constant: i64Like) -> None:
        pass

    @classmethod
    def var(cls, name: StringLike) -> Exp:
        pass

    def __sub__(self, other: Exp) -> Exp:
        pass

    def __add__(self, other: Exp) -> Exp:
        pass

    def __neg__(self) -> Exp:
        pass


@egraph.class_
class Binop(Expr):
    LESS: ClassVar[Binop]
    EQUAL: ClassVar[Binop]


@egraph.class_
class Cond(Expr):
    def __init__(self, binop: Binop, expr1: Exp, expr2: Exp) -> None:
        pass


@egraph.class_
class Cmd(Expr):
    @classmethod
    def assume(cls, cond: Cond) -> Cmd:
        pass

    @classmethod
    def choice(cls, cmd1: Cmd, cmd2: Cmd) -> Cmd:
        pass

    @classmethod
    def while_(cls, cond: Cond, cmd: Cmd) -> Cmd:
        pass

    @classmethod
    def assign(cls, var_: StringLike, expr_: Exp) -> Cmd:
        pass

    def __add__(self, cmd: Cmd) -> Cmd:
        pass


expr = (
    Cmd.assign("x", Exp(0))
    + Cmd.assign("y", Exp(2))
    + Cmd.while_(
        Cond(Binop.LESS, Exp.var("x"), Exp(2)),
        Cmd.assign("x", Exp.var("x") + Exp(1)) + Cmd.assign("x", Exp(5)),
    )
)

egraph.register(expr)
egraph.display()


@egraph.class_
class SignDomain(Expr):
    def __init__(self, negative: BoolLike, zero: BoolLike, positive: BoolLike) -> None:
        pass

    def join(self, other: SignDomain) -> SignDomain:
        pass

    def meet(self, other: SignDomain) -> SignDomain:
        pass


TOP = SignDomain(True, True, True)
BOTTOM = SignDomain(False, False, False)


@egraph.register
def _sign_domain(
    neg1: Bool, zero1: Bool, pos1: Bool, neg2: Bool, zero2: Bool, pos2: Bool
):
    yield rewrite(SignDomain(neg1, zero1, pos1).join(SignDomain(neg2, zero2, pos2))).to(
        SignDomain(neg1 | neg2, zero1 | zero2, pos1 | pos2)
    )
    yield rewrite(SignDomain(neg1, zero1, pos1).meet(SignDomain(neg2, zero2, pos2))).to(
        SignDomain(neg1 & neg2, zero1 & zero2, pos1 & pos2)
    )


Table = Map[String, SignDomain]


@egraph.class_
class Memory(Expr):
    def __init__(self, table: Table = Table.empty()) -> None:
        pass

    def __getitem__(self, key: StringLike) -> SignDomain:
        """
        Return the SignDomain for the given variable, if none is found, return TOP.
        """


@egraph.register
def _memory(t: Table, key: String):
    yield rewrite(Memory(t)[key]).to(t[key], t, t.contains(key))
    yield rewrite(Memory(t)[key]).to(TOP, t, t.not_contains(key))


with egraph:
    not_found = Memory()["hi"]
    found = Memory(Table.empty().insert(String("hi"), BOTTOM))["hi"]
    egraph.register(not_found, found)
    egraph.run(1)
    egraph.check(eq(not_found).to(TOP))
    egraph.check(eq(found).to(BOTTOM))


@egraph.function
def sign(x: Exp, mem: Memory) -> SignDomain:
    ...


@egraph.function
def interp(cmd: Cmd, mem: Memory = Memory()) -> Memory:
    ...


# @egraph.register
# def _interp():
#     yield
