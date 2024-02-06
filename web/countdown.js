function solve(target, numbers) {
    let terms = Array.from(findTerms(numbers, {}));
    console.log(terms.length);
    let relevantTerms = terms.filter(e => Math.abs(e.v - target) <= 10);
    console.log(relevantTerms.length);
    let groupedTerms = Object.groupBy(relevantTerms, e => Math.abs(e.v - target));
    return groupedTerms;
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
    return ({ type: 'single', v: i });
}

function areEqual(term1, term2) {
    // TODO: this is fast, but doesn't find all solutions
    if (term1.v == term2.v) {
        if (term1.type === 'single' && term2.type === 'single') {
            return true;
        }
        if (term1.type === 'term' && term2.type === 'term') {
            return term1.op === term2.op; //&& areEqual(term1.left, term2.left) && areEqual(term1.right, term2.right);
        }
    }
    return false;
}

function findTerms(numbers, memo) {
    if (memo[numbers]) {
        return memo[numbers];
    }
    let ops = ['+', '-', '*', '/'];
    let subdivisions = subdivide(numbers);
    let result = numbers.map(e => toSingle(e));
    for (let op of ops) {
        for (let subdivision of subdivisions) {
            let [left, right] = subdivision;
            let leftTerms = findTerms(left, memo);
            let rightTerms = findTerms(right, memo);
            for (let leftTerm of leftTerms) {
                for (let rightTerm of rightTerms) {
                    let v = evaluate(op, leftTerm, rightTerm);
                    if (v) {
                        let t = ({ type: 'term', op: op, left: leftTerm, right: rightTerm, v: v });
                        if (!result.some(e => areEqual(e, t))) {
                            result.push(t);
                        }
                    }
                }
            }
        }
    }
    memo[numbers] = result;
    return result;
}

function show(term) {
    if (term.type === 'single') {
        return term.v.toString();
    }
    return '(' + show(term.left) + term.op + show(term.right) + ')';
}