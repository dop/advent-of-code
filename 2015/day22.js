const assert = require('assert')
const fs = require('fs')

let [boss_points, boss_damage] = fs.readFileSync(0, {encoding: 'utf-8'})
  .match(/\d+/g).map(Number)

let best_mana = Infinity

function remember(next_state, previous_state) {
  return next_state
  // const {history, ...state} = previous_state
  // return {...next_state, history: [...next_state.history, state]}
}

function apply_effects({effects, player, boss, mana_limit, ...state}) {
  if (effects.shield > 0) {
    effects = { ...effects, shield: effects.shield - 1 }
    player = {...player, armor: 7}
  } else {
    player = {...player, armor: 0}
  }

  if (effects.poison > 0) {
    effects = { ...effects, poison: effects.poison - 1 }
    boss = {...boss, points: boss.points - 3}
  }

  if (effects.recharge > 0) {
    effects = { ...effects, recharge: effects.recharge - 1 }
    mana_limit += 101
  }

  return remember({...state, effects, boss, player, mana_limit}, state)
}

function boss_attack(state) {
  return remember({
    ...state,
    player: {
      ...state.player,
      points: state.player.points - Math.max(1, state.boss.damage - state.player.armor),
    },
  }, state)
}

function branch(state) {
  const {mana, mana_limit, effects, boss, player} = state
  let states = []

  if (mana > best_mana)
    return []

  // magic missile
  if (mana + 53 <= mana_limit) {
    states.push({...state, boss: {...boss, points: boss.points - 4}, mana: mana + 53})
  }
  // drain
  if (mana + 73 <= mana_limit) {
    states.push({
      ...state,
      mana: mana + 73,
      boss: {...boss, points: boss.points - 2},
      player: {...player, points: player.points + 2},
    })
  }
  // shield
  if (mana + 113 <= mana_limit && effects.shield <= 0) {
    states.push({...state, effects: {...effects, shield: 6}, mana: mana + 113})
  }
  // poison
  if (mana + 173 <= mana_limit && effects.poison <= 0) {
    states.push({...state, effects: {...effects, poison: 6}, mana: mana + 173})
  }
  // recharge
  if (mana + 229 <= mana_limit && effects.recharge <= 0) {
    states.push({...state, effects: {...effects, recharge: 5}, mana: mana + 229})
  }
  return states.map(ns => remember(ns, state))
}

function run(state) {
  state.player.points -= 1
  if (state.player.points <= 0)
    return Infinity

  return branch(state)
    .map(next_state => {
      next_state = apply_effects(next_state)
      if (next_state.boss.points <= 0) {
        // console.log('win', next_state.mana)
        best_mana = Math.min(best_mana, next_state.mana)
        return next_state.mana
      }
      next_state = boss_attack(next_state)
      if (next_state.player.points <= 0) {
        // console.log('lost', next_state.mana)
        return Infinity
      }
      return run(apply_effects(next_state))
    })
    .reduce((best, candidate) => best < candidate ? best : candidate, Infinity)
}

console.log( run({
  effects: {
    shield: 0,
    poison: 0,
    recharge: 0,
  },
  player: {
    armor: 0,
    points: 50,
  },
  boss: {
    points: boss_points,
    damage: boss_damage,
  },
  mana: 0,
  mana_limit: 500,
  history: [],
}) )
