from enum import StrEnum

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

    def get_heuristic(self, mode: DomainMode = DomainMode.UNIT):
        match (self, mode):
            case self.PANCAKE, DomainMode.UNIT:
                return gap_unit
            case self.PANCAKE, DomainMode.ARBITRARY:
                return gap_arbitrary
            case self.SLIDINGTILE, DomainMode.UNIT:
                return manhattan_unit
            case self.SLIDINGTILE, DomainMode.ARBITRARY:
                return manhattan_arbitrary
            case self.CYCLICTILE, DomainMode.UNIT:
                return cyclic_manhattan_unit
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
