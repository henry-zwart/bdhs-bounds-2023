// pub trait State {
//     fn applicable_operations(&self) -> Vec<TileMove>;
//     fn actions(&self) -> Vec<TileAction>;
// }


// #[derive(Debug)]
// pub struct Tiles {
//     tile_by_pos: [usize;9],
//     pos_by_tile: [usize;9],
// }

// impl Tiles {
//     fn new(tile_by_pos: [usize;9], pos_by_tile: [usize;9]) -> Self {
//         Tiles { tile_by_pos: tile_by_pos, pos_by_tile: pos_by_tile }
//     }

//     fn from_swap(original_tiles: &Self, pos1: usize, pos2: usize) -> Self {
//         let mut new_tile_by_pos = original_tiles.tile_by_pos;
//         new_tile_by_pos.swap(pos1.into(), pos2.into());
//         Tiles::from_positions(new_tile_by_pos)
//     }

//     fn from_tiles(tile_by_pos: [usize;9]) -> Self {
//         let mut pos_by_tile = [0;9];
//         for (pos, tile) in tile_by_pos.iter().enumerate() {
//             pos_by_tile[*tile] = pos
//         }
//         Tiles::new(tile_by_pos, pos_by_tile)
//     }

//     fn from_positions(pos_by_tile: [usize;9]) -> Self {
//         let mut tile_by_pos = [0;9];
//         for (tile, pos) in pos_by_tile.iter().enumerate() {
//             tile_by_pos[*pos] = tile
//         }
//         Tiles::new(tile_by_pos, pos_by_tile)
//     }
// }

// #[derive(Debug)]
// pub enum TileMove {
//     Up,
//     Down, 
//     Left, 
//     Right,
// }

// impl TileMove {
//     fn applicable(&self, state: &TileState) -> bool {
//         match state {
//             TileState::Cyclic(_) => true,
//             TileState::Standard(tiles) => {
//                 let blank_pos = tiles.pos_by_tile[0];
//                 match self {
//                     TileMove::Up => (blank_pos / 3) != 0,
//                     TileMove::Right => (blank_pos % 3) != 2,
//                     TileMove::Down => (blank_pos / 3) != 2,
//                     TileMove::Left => (blank_pos % 3) != 0,
//                 }
//             }
//         }
//     }

//     fn result(&self, state: &TileState) -> TileState {
//         let tiles = match state {
//             TileState::Standard(tiles) | TileState::Cyclic(tiles) => tiles
//         };
//         let blank_pos = tiles.pos_by_tile[0];
//         let new_pos = match self {
//             TileMove::Up => {
//                 ((blank_pos+9) - 3) % 9
//             },
//             TileMove::Right => {
//                 3 * (blank_pos / 3) + ((blank_pos + 1) % 3)
//             },
//             TileMove::Down => {
//                 (blank_pos + 3) % 9
//             },
//             TileMove::Left => {
//                 3 * (blank_pos / 3) + (((blank_pos+3) - 1) % 3)
//             },
//         };
//         let new_tiles = Tiles::from_swap(tiles, blank_pos, new_pos);
//         match state {
//             TileState::Standard(_) => TileState::Standard(new_tiles),
//             TileState::Cyclic(_) => TileState::Cyclic(new_tiles)
//         }
//     }
// }

// #[derive(Debug)]
// pub enum TileAction<'a> {
//     Slide(&'a TileState, TileMove),
// }

// impl TileAction<'_> {
//     fn apply(&self) -> TileState {
//         match self {
//             Self::Slide(state, mv) => mv.result(state)
//         }
//     }
// }

// #[derive(Debug)]
// pub enum TileState {
//     Standard(Tiles),
//     Cyclic(Tiles),
// }

// impl State for TileState {

//     fn applicable_operations(&self) -> Vec<TileMove> {
//         let mut ops = Vec::with_capacity(4);
//         for mv in [TileMove::Up, TileMove::Right, TileMove::Down, TileMove::Left].into_iter() {
//             if mv.applicable(self) {
//                 ops.push(mv)
//             }
//         }
//         ops
//     } 

//     fn actions(self: &Self) -> Vec<TileAction> {
//         let applicable_ops = self.applicable_operations();
//         applicable_ops.into_iter().map(|mv| TileAction::Slide(self,mv)).collect()
//     }
// }