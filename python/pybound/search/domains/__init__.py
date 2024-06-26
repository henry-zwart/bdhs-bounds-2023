from enum import StrEnum
from functools import partial

from .cyclictile import (
    CyclicTileDomainArbitrary,
    CyclicTileDomainUnit,
    cyclic_manhattan_arbitrary,
    cyclic_manhattan_unit,
)
from .domain import Domain
from .pancake import PancakeDomainArbitrary, PancakeDomainUnit, gap_arbitrary, gap_unit
from .tile import (
    TileDomainArbitrary,
    TileDomainUnit,
    manhattan_arbitrary,
    manhattan_unit,
)


class DomainMode(StrEnum):
    UNIT = "unit"
    ARBITRARY = "arbitrary"


class DomainType(StrEnum):
    PANCAKE = "pancake"
    SLIDINGTILE = "slidingtile"
    CYCLICTILE = "cyclictile"

    def get_domain(self, mode: DomainMode = DomainMode.UNIT) -> Domain:
        match (self, mode):
            case self.PANCAKE, DomainMode.UNIT:
                return PancakeDomainUnit
            case self.PANCAKE, DomainMode.ARBITRARY:
                return PancakeDomainArbitrary
            case self.SLIDINGTILE, DomainMode.UNIT:
                return TileDomainUnit
            case self.SLIDINGTILE, DomainMode.ARBITRARY:
                return TileDomainArbitrary
            case self.CYCLICTILE, DomainMode.UNIT:
                return CyclicTileDomainUnit
            case self.CYCLICTILE, DomainMode.ARBITRARY:
                return CyclicTileDomainArbitrary

    def get_heuristic(self, mode: DomainMode = DomainMode.UNIT, degradation=0):
        match (self, mode):
            case self.PANCAKE, DomainMode.UNIT:
                return partial(gap_unit, degradation=degradation)
            case self.PANCAKE, DomainMode.ARBITRARY:
                return partial(gap_arbitrary, degradation=degradation)
            case self.SLIDINGTILE, DomainMode.UNIT:
                return partial(manhattan_unit, degradation=degradation)
            case self.SLIDINGTILE, DomainMode.ARBITRARY:
                return manhattan_arbitrary
            case self.CYCLICTILE, DomainMode.UNIT:
                return partial(cyclic_manhattan_unit, degradation=degradation)
            case self.CYCLICTILE, DomainMode.ARBITRARY:
                return cyclic_manhattan_arbitrary

    def get_heuristic_name(self):
        match self:
            case self.PANCAKE:
                return "gap"
            case self.SLIDINGTILE:
                return "manhattan"
            case self.CYCLICTILE:
                return "cyclic_manhattan"
