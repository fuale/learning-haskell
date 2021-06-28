const util = require('util')
const log = x => {
    console.log(util.inspect(x, false, null, true))
    console.log("\n".padStart(44, '-'))
}

function change(sum, coins = []) {

    const r = []

    const solution = (sum, l = [], c = []) => {
        if (sum < 0) {
            return [];
        }

        if (sum === 0) {
            return [l];
        }

        return coins.flatMap(coin => {
            return solution(sum - coin, l.concat(coin))
        })
    }

    log(
        solution(sum)
    )

    return r;
}

log(
    change(7, [2, 3, 7])
)