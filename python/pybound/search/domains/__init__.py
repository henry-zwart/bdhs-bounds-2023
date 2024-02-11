from enum import StrEnum

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
