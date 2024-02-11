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
    SLIDING_TILE = "sliding-tile"

    def get_domain(self, mode: DomainMode = DomainMode.UNIT) -> Domain:
        match (self, mode):
            case self.PANCAKE, DomainMode.UNIT:
                return PancakeDomainUnit
            case self.PANCAKE, DomainMode.ARBITRARY:
                return PancakeDomainArbitrary
            case self.SLIDING_TILE, DomainMode.UNIT:
                return TileDomainUnit
            case self.SLIDING_TILE, DomainMode.ARBITRARY:
                return TileDomainArbitrary

    def get_heuristic(self, mode: DomainMode = DomainMode.UNIT):
        match (self, mode):
            case self.PANCAKE, DomainMode.UNIT:
                return gap_unit
            case self.PANCAKE, DomainMode.ARBITRARY:
                return gap_arbitrary
            case self.SLIDING_TILE, DomainMode.UNIT:
                return manhattan_unit
            case self.SLIDING_TILE, DomainMode.ARBITRARY:
                return manhattan_arbitrary
