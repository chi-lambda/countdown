function solve(target, numbers) {
    let terms = findTerms(numbers.toReversed(), {});
    let legalTerms = terms.filter(t => 100 <= t.v && t.v <= 999);
    return legalTerms;
}

function subdivide(numbers) {
    let len = numbers.length;
    let num = Math.pow(2, len);
    let result = [];
    for (let i = 1; i < num - 1; i++) {
        result.push(bitsplit(numbers, i))
    }
    return result;
}

function bitsplit(l, n) {
    let left = [];
    let right = [];
    for (let i = 0; i < l.length; i++) {
        if (n % 2 == 0) {
            left.push(l[i]);
        } else {
            right.push(l[i]);
        }
        n = Math.floor(n / 2);
    }
    return [left, right];
}

function evaluate(op, left, right) {
    switch (op) {
        case '+':
            return left.v >= right.v ? left.v + right.v : null;
        case '-':
            return left.v > right.v ? left.v - right.v : null;
        case '*':
            return (left.v > 1 && right.v > 1 && left.v >= right.v) ? left.v * right.v : null;
        case '/':
            return (right.v > 1 && left.v % right.v == 0) ? left.v / right.v : null;
    }
}

function toSingle(i) {
    return ({ type: 'single', v: i, size: 1 });
}

function findTerms(numbers, memo) {
    if (memo[numbers]) {
        return memo[numbers];
    }
    let ops = ['+', '-', '*', '/'];
    let subdivisions = subdivide(numbers);
    let result = {}
    for (let n of numbers) {
        result[n] = toSingle(n);
    }
    for (let op of ops) {
        for (let subdivision of subdivisions) {
            let [left, right] = subdivision;
            let leftTerms = findTerms(left, memo);
            let rightTerms = findTerms(right, memo);
            for (let leftTerm of leftTerms) {
                for (let rightTerm of rightTerms) {
                    let v = evaluate(op, leftTerm, rightTerm);
                    if (v) {
                        let t = ({ type: 'term', op: op, left: leftTerm, right: rightTerm, v: v, size: leftTerm.size + rightTerm.size });
                        if (!result[v] || result[v].size > t.size) {
                            result[v] = t;
                        }
                    }
                }
            }
        }
    }
    let resultArray = Object.values(result);
    memo[numbers] = resultArray;
    return resultArray;
}

function findSummands(term) {
    if (term.type === 'single' || term.op !== '+') {
        return [term];
    }
    return findSummands(term.left).concat(findSummands(term.right));
}

function show(term) {
    if (term.type === 'single') {
        return term.v.toString();
    }
    if (term.op === '+') {
        let summands = findSummands(term);
        let sum = summands.map(t => t.v).reduce((a, b) => a + b);
        return summands.map(t => t.type === 'single' ? show(t) : '(' + show(t) + ')').join(' + ') + ' = ' + sum;
    }
    let left = term.left.type === 'single' ? show(term.left) : '(' + show(term.left) + ')';
    let right = term.right.type === 'single' ? show(term.right) : '(' + show(term.right) + ')';
    return left + ' ' + term.op + ' ' + right + ' = ' + evaluate(term.op, term.left, term.right);
}
