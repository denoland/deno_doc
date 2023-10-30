// deno-fmt-ignore-file
// deno-lint-ignore-file
// This code was bundled using `deno bundle` and it's not recommended to edit it manually

const STEMMERS = {
    arabic: 'ar',
    armenian: 'am',
    bulgarian: 'bg',
    danish: 'dk',
    dutch: 'nl',
    english: 'en',
    finnish: 'fi',
    french: 'fr',
    german: 'de',
    greek: 'gr',
    hungarian: 'hu',
    indian: 'in',
    indonesian: 'id',
    irish: 'ie',
    italian: 'it',
    lithuanian: 'lt',
    nepali: 'np',
    norwegian: 'no',
    portuguese: 'pt',
    romanian: 'ro',
    russian: 'ru',
    serbian: 'rs',
    slovenian: 'ru',
    spanish: 'es',
    swedish: 'se',
    tamil: 'ta',
    turkish: 'tr',
    ukrainian: 'uk',
    sanskrit: 'sk'
};
const SPLITTERS = {
    dutch: /[^A-Za-zàèéìòóù0-9_'-]+/gim,
    english: /[^A-Za-zàèéìòóù0-9_'-]+/gim,
    french: /[^a-z0-9äâàéèëêïîöôùüûœç-]+/gim,
    italian: /[^A-Za-zàèéìòóù0-9_'-]+/gim,
    norwegian: /[^a-z0-9_æøåÆØÅäÄöÖüÜ]+/gim,
    portuguese: /[^a-z0-9à-úÀ-Ú]/gim,
    russian: /[^a-z0-9а-яА-ЯёЁ]+/gim,
    spanish: /[^a-z0-9A-Zá-úÁ-ÚñÑüÜ]+/gim,
    swedish: /[^a-z0-9_åÅäÄöÖüÜ-]+/gim,
    german: /[^a-z0-9A-ZäöüÄÖÜß]+/gim,
    finnish: /[^a-z0-9äöÄÖ]+/gim,
    danish: /[^a-z0-9æøåÆØÅ]+/gim,
    hungarian: /[^a-z0-9áéíóöőúüűÁÉÍÓÖŐÚÜŰ]+/gim,
    romanian: /[^a-z0-9ăâîșțĂÂÎȘȚ]+/gim,
    serbian: /[^a-z0-9čćžšđČĆŽŠĐ]+/gim,
    turkish: /[^a-z0-9çÇğĞıİöÖşŞüÜ]+/gim,
    lithuanian: /[^a-z0-9ąčęėįšųūžĄČĘĖĮŠŲŪŽ]+/gim,
    arabic: /[^a-z0-9أ-ي]+/gim,
    nepali: /[^a-z0-9अ-ह]+/gim,
    irish: /[^a-z0-9áéíóúÁÉÍÓÚ]+/gim,
    indian: /[^a-z0-9अ-ह]+/gim,
    armenian: /[^a-z0-9ա-ֆ]+/gim,
    greek: /[^a-z0-9α-ωά-ώ]+/gim,
    indonesian: /[^a-z0-9]+/gim,
    ukrainian: /[^a-z0-9а-яА-ЯіїєІЇЄ]+/gim,
    slovenian: /[^a-z0-9čžšČŽŠ]+/gim,
    bulgarian: /[^a-z0-9а-яА-Я]+/gim,
    tamil: /[^a-z0-9அ-ஹ]+/gim,
    sanskrit: /[^a-z0-9A-Zāīūṛḷṃṁḥśṣṭḍṇṅñḻḹṝ]+/gim
};
const SUPPORTED_LANGUAGES = Object.keys(STEMMERS);
const allLanguages = SUPPORTED_LANGUAGES.join('\n - ');
const baseId = Date.now().toString().slice(5);
const errors = {
    NO_LANGUAGE_WITH_CUSTOM_TOKENIZER: 'Do not pass the language option to create when using a custom tokenizer.',
    LANGUAGE_NOT_SUPPORTED: `Language "%s" is not supported.\nSupported languages are:\n - ${allLanguages}`,
    INVALID_STEMMER_FUNCTION_TYPE: `config.stemmer property must be a function.`,
    MISSING_STEMMER: `As of version 1.0.0 @orama/orama does not ship non English stemmers by default. To solve this, please explicitly import and specify the "%s" stemmer from the package @orama/stemmers. See https://docs.oramasearch.com/open-source/text-analysis/stemming for more information.`,
    CUSTOM_STOP_WORDS_MUST_BE_FUNCTION_OR_ARRAY: 'Custom stop words array must only contain strings.',
    UNSUPPORTED_COMPONENT: `Unsupported component "%s".`,
    COMPONENT_MUST_BE_FUNCTION: `The component "%s" must be a function.`,
    COMPONENT_MUST_BE_FUNCTION_OR_ARRAY_FUNCTIONS: `The component "%s" must be a function or an array of functions.`,
    INVALID_SCHEMA_TYPE: `Unsupported schema type "%s" at "%s". Expected "string", "boolean" or "number" or array of them.`,
    DOCUMENT_ID_MUST_BE_STRING: `Document id must be of type "string". Got "%s" instead.`,
    DOCUMENT_ALREADY_EXISTS: `A document with id "%s" already exists.`,
    DOCUMENT_DOES_NOT_EXIST: `A document with id "%s" does not exists.`,
    MISSING_DOCUMENT_PROPERTY: `Missing searchable property "%s".`,
    INVALID_DOCUMENT_PROPERTY: `Invalid document property "%s": expected "%s", got "%s"`,
    UNKNOWN_INDEX: `Invalid property name "%s". Expected a wildcard string ("*") or array containing one of the following properties: %s`,
    INVALID_BOOST_VALUE: `Boost value must be a number greater than, or less than 0.`,
    INVALID_FILTER_OPERATION: `You can only use one operation per filter, you requested %d.`,
    SCHEMA_VALIDATION_FAILURE: `Cannot insert document due schema validation failure on "%s" property.`,
    INVALID_SORT_SCHEMA_TYPE: `Unsupported sort schema type "%s" at "%s". Expected "string" or "number".`,
    CANNOT_SORT_BY_ARRAY: `Cannot configure sort for "%s" because it is an array (%s).`,
    UNABLE_TO_SORT_ON_UNKNOWN_FIELD: `Unable to sort on unknown field "%s". Allowed fields: %s`,
    SORT_DISABLED: `Sort is disabled. Please read the documentation at https://docs.oramasearch for more information.`,
    UNKNOWN_GROUP_BY_PROPERTY: `Unknown groupBy property "%s".`,
    INVALID_GROUP_BY_PROPERTY: `Invalid groupBy property "%s". Allowed types: "%s", but given "%s".`,
    UNKNOWN_FILTER_PROPERTY: `Unknown filter property "%s".`,
    INVALID_VECTOR_SIZE: `Vector size must be a number greater than 0. Got "%s" instead.`,
    INVALID_VECTOR_VALUE: `Vector value must be a number greater than 0. Got "%s" instead.`,
    INVALID_INPUT_VECTOR: `Property "%s" was declared as a %s-dimensional vector, but got a %s-dimensional vector instead.\nInput vectors must be of the size declared in the schema, as calculating similarity between vectors of different sizes can lead to unexpected results.`,
    WRONG_SEARCH_PROPERTY_TYPE: `Property "%s" is not searchable. Only "string" properties are searchable.`,
    FACET_NOT_SUPPORTED: `Facet doens't support the type "%s".`,
    INVALID_DISTANCE_SUFFIX: `Invalid distance suffix "%s". Valid suffixes are: cm, m, km, mi, yd, ft.`
};
let lastId = 0;
const nano = BigInt(1e3);
const milli = BigInt(1e6);
const second = BigInt(1e9);
typeof window === 'undefined';
const MAX_ARGUMENT_FOR_STACK = 65535;
function safeArrayPush(arr, newArr) {
    if (newArr.length < 65535) {
        Array.prototype.push.apply(arr, newArr);
    } else {
        for(let i = 0; i < newArr.length; i += MAX_ARGUMENT_FOR_STACK){
            Array.prototype.push.apply(arr, newArr.slice(i, i + 65535));
        }
    }
}
function sprintf(template, ...args) {
    return template.replace(/%(?:(?<position>\d+)\$)?(?<width>-?\d*\.?\d*)(?<type>[dfs])/g, function(...replaceArgs) {
        const groups = replaceArgs[replaceArgs.length - 1];
        const { width: rawWidth, type, position } = groups;
        const replacement = position ? args[Number.parseInt(position) - 1] : args.shift();
        const width = rawWidth === '' ? 0 : Number.parseInt(rawWidth);
        switch(type){
            case 'd':
                return replacement.toString().padStart(width, '0');
            case 'f':
                {
                    let value = replacement;
                    const [padding, precision] = rawWidth.split('.').map((w)=>Number.parseFloat(w));
                    if (typeof precision === 'number' && precision >= 0) {
                        value = value.toFixed(precision);
                    }
                    return typeof padding === 'number' && padding >= 0 ? value.toString().padStart(width, '0') : value.toString();
                }
            case 's':
                return width < 0 ? replacement.toString().padEnd(-width, ' ') : replacement.toString().padStart(width, ' ');
            default:
                return replacement;
        }
    });
}
function createError(code, ...args) {
    const error = new Error(sprintf(errors[code] ?? `Unsupported Orama Error code: ${code}`, ...args));
    error.code = code;
    if ('captureStackTrace' in Error.prototype) {
        Error.captureStackTrace(error);
    }
    return error;
}
async function formatNanoseconds(value) {
    if (typeof value === 'number') {
        value = BigInt(value);
    }
    if (value < nano) {
        return `${value}ns`;
    } else if (value < milli) {
        return `${value / nano}μs`;
    } else if (value < second) {
        return `${value / milli}ms`;
    }
    return `${value / second}s`;
}
async function getNanosecondsTime() {
    if (typeof process !== 'undefined' && process.hrtime !== undefined) {
        return process.hrtime.bigint();
    }
    if (typeof performance !== 'undefined') {
        return BigInt(Math.floor(performance.now() * 1e6));
    }
    return BigInt(0);
}
async function uniqueId() {
    return `${baseId}-${lastId++}`;
}
function getOwnProperty(object, property) {
    if (Object.hasOwn === undefined) {
        return Object.prototype.hasOwnProperty.call(object, property) ? object[property] : undefined;
    }
    return Object.hasOwn(object, property) ? object[property] : undefined;
}
function sortTokenScorePredicate(a, b) {
    if (b[1] === a[1]) {
        return a[0] - b[0];
    }
    return b[1] - a[1];
}
function intersect(arrays) {
    if (arrays.length === 0) {
        return [];
    } else if (arrays.length === 1) {
        return arrays[0];
    }
    for(let i = 1; i < arrays.length; i++){
        if (arrays[i].length < arrays[0].length) {
            const tmp = arrays[0];
            arrays[0] = arrays[i];
            arrays[i] = tmp;
        }
    }
    const set = new Map();
    for (const elem of arrays[0]){
        set.set(elem, 1);
    }
    for(let i = 1; i < arrays.length; i++){
        let found = 0;
        for (const elem of arrays[i]){
            const count = set.get(elem);
            if (count === i) {
                set.set(elem, count + 1);
                found++;
            }
        }
        if (found === 0) return [];
    }
    return arrays[0].filter((e)=>{
        const count = set.get(e);
        if (count !== undefined) set.set(e, 0);
        return count === arrays.length;
    });
}
async function getDocumentProperties(doc, paths) {
    const properties = {};
    const pathsLength = paths.length;
    for(let i = 0; i < pathsLength; i++){
        const path = paths[i];
        const pathTokens = path.split('.');
        let current = doc;
        const pathTokensLength = pathTokens.length;
        for(let j = 0; j < pathTokensLength; j++){
            current = current[pathTokens[j]];
            if (typeof current === 'object') {
                if (current !== null && 'lat' in current && 'lon' in current && typeof current.lat === 'number' && typeof current.lon === 'number') {
                    current = properties[path] = current;
                    break;
                } else if (!Array.isArray(current) && current !== null && j === pathTokensLength - 1) {
                    current = undefined;
                    break;
                }
            } else if ((current === null || typeof current !== 'object') && j < pathTokensLength - 1) {
                current = undefined;
                break;
            }
        }
        if (typeof current !== 'undefined') {
            properties[path] = current;
        }
    }
    return properties;
}
async function getNested(obj, path) {
    const props = await getDocumentProperties(obj, [
        path
    ]);
    return props[path];
}
const mapDistanceToMeters = {
    cm: 0.01,
    m: 1,
    km: 1000,
    ft: 0.3048,
    yd: 0.9144,
    mi: 1609.344
};
function convertDistanceToMeters(distance, unit) {
    const ratio = mapDistanceToMeters[unit];
    if (ratio === undefined) {
        throw new Error(createError('INVALID_DISTANCE_SUFFIX', distance).message);
    }
    return distance * ratio;
}
async function formatElapsedTime(n) {
    return {
        raw: Number(n),
        formatted: await formatNanoseconds(n)
    };
}
async function getDocumentIndexId(doc) {
    if (doc.id) {
        if (typeof doc.id !== 'string') {
            throw createError('DOCUMENT_ID_MUST_BE_STRING', typeof doc.id);
        }
        return doc.id;
    }
    return await uniqueId();
}
async function validateSchema(doc, schema) {
    for (const [prop, type] of Object.entries(schema)){
        const value = doc[prop];
        if (typeof value === 'undefined') {
            continue;
        }
        if (type === 'geopoint' && typeof value === 'object' && typeof value.lon === 'number' && typeof value.lat === 'number') {
            continue;
        }
        if (type === 'enum' && (typeof value === 'string' || typeof value === 'number')) {
            continue;
        }
        if (type === 'enum[]' && Array.isArray(value)) {
            const valueLength = value.length;
            for(let i = 0; i < valueLength; i++){
                if (typeof value[i] !== 'string' && typeof value[i] !== 'number') {
                    return prop + '.' + i;
                }
            }
            continue;
        }
        if (isVectorType(type)) {
            const vectorSize = getVectorSize(type);
            if (!Array.isArray(value) || value.length !== vectorSize) {
                throw createError('INVALID_INPUT_VECTOR', prop, vectorSize, value.length);
            }
            continue;
        }
        if (isArrayType(type)) {
            if (!Array.isArray(value)) {
                return prop;
            }
            const expectedType = getInnerType(type);
            const valueLength = value.length;
            for(let i = 0; i < valueLength; i++){
                if (typeof value[i] !== expectedType) {
                    return prop + '.' + i;
                }
            }
            continue;
        }
        if (typeof type === 'object') {
            if (!value || typeof value !== 'object') {
                return prop;
            }
            const subProp = await validateSchema(value, type);
            if (subProp) {
                return prop + '.' + subProp;
            }
            continue;
        }
        if (typeof value !== type) {
            return prop;
        }
    }
    return undefined;
}
const IS_ARRAY_TYPE = {
    string: false,
    number: false,
    boolean: false,
    enum: false,
    geopoint: false,
    'string[]': true,
    'number[]': true,
    'boolean[]': true,
    'enum[]': true
};
const INNER_TYPE = {
    'string[]': 'string',
    'number[]': 'number',
    'boolean[]': 'boolean',
    'enum[]': 'enum'
};
function isGeoPointType(type) {
    return type === 'geopoint';
}
function isVectorType(type) {
    return typeof type === 'string' && /^vector\[\d+\]$/.test(type);
}
function isArrayType(type) {
    return typeof type === 'string' && IS_ARRAY_TYPE[type];
}
function getInnerType(type) {
    return INNER_TYPE[type];
}
function getVectorSize(type) {
    const size = Number(type.slice(7, -1));
    switch(true){
        case isNaN(size):
            throw createError('INVALID_VECTOR_VALUE', type);
        case size <= 0:
            throw createError('INVALID_VECTOR_SIZE', type);
        default:
            return size;
    }
}
function createInternalDocumentIDStore() {
    return {
        idToInternalId: new Map(),
        internalIdToId: [],
        save,
        load
    };
}
function save(store) {
    return {
        internalIdToId: store.internalIdToId
    };
}
function load(orama, raw) {
    const { internalIdToId } = raw;
    orama.internalDocumentIDStore.idToInternalId.clear();
    orama.internalDocumentIDStore.internalIdToId = [];
    for(let i = 0; i < internalIdToId.length; i++){
        orama.internalDocumentIDStore.idToInternalId.set(internalIdToId[i], i + 1);
        orama.internalDocumentIDStore.internalIdToId.push(internalIdToId[i]);
    }
}
function getInternalDocumentId(store, id) {
    if (typeof id === 'string') {
        const internalId = store.idToInternalId.get(id);
        if (internalId) {
            return internalId;
        }
        const currentId = store.idToInternalId.size + 1;
        store.idToInternalId.set(id, currentId);
        store.internalIdToId.push(id);
        return currentId;
    }
    if (id > store.internalIdToId.length) {
        return getInternalDocumentId(store, id.toString());
    }
    return id;
}
function getDocumentIdFromInternalId(store, internalId) {
    if (store.internalIdToId.length < internalId) {
        throw new Error(`Invalid internalId ${internalId}`);
    }
    return store.internalIdToId[internalId - 1];
}
async function create(_, sharedInternalDocumentStore) {
    return {
        sharedInternalDocumentStore,
        docs: {},
        count: 0
    };
}
async function get(store, id) {
    const internalId = getInternalDocumentId(store.sharedInternalDocumentStore, id);
    return store.docs[internalId];
}
async function getMultiple(store, ids) {
    const found = Array.from({
        length: ids.length
    });
    for(let i = 0; i < ids.length; i++){
        const internalId = getInternalDocumentId(store.sharedInternalDocumentStore, ids[i]);
        found[i] = store.docs[internalId];
    }
    return found;
}
async function getAll(store) {
    return store.docs;
}
async function store(store, id, doc) {
    const internalId = getInternalDocumentId(store.sharedInternalDocumentStore, id);
    if (typeof store.docs[internalId] !== 'undefined') {
        return false;
    }
    store.docs[internalId] = doc;
    store.count++;
    return true;
}
async function remove(store, id) {
    const internalId = getInternalDocumentId(store.sharedInternalDocumentStore, id);
    if (typeof store.docs[internalId] === 'undefined') {
        return false;
    }
    delete store.docs[internalId];
    store.count--;
    return true;
}
async function count(store) {
    return store.count;
}
async function load1(sharedInternalDocumentStore, raw) {
    const rawDocument = raw;
    return {
        docs: rawDocument.docs,
        count: rawDocument.count,
        sharedInternalDocumentStore
    };
}
async function save1(store) {
    return {
        docs: store.docs,
        count: store.count
    };
}
async function createDocumentsStore() {
    return {
        create,
        get,
        getMultiple,
        getAll,
        store,
        remove,
        count,
        load: load1,
        save: save1
    };
}
const OBJECT_COMPONENTS = [
    'tokenizer',
    'index',
    'documentsStore',
    'sorter'
];
const FUNCTION_COMPONENTS = [
    'validateSchema',
    'getDocumentIndexId',
    'getDocumentProperties',
    'formatElapsedTime'
];
const SINGLE_OR_ARRAY_COMPONENTS = [
    'beforeInsert',
    'afterInsert',
    'beforeRemove',
    'afterRemove',
    'beforeUpdate',
    'afterUpdate',
    'afterSearch',
    'beforeMultipleInsert',
    'afterMultipleInsert',
    'beforeMultipleRemove',
    'afterMultipleRemove',
    'beforeMultipleUpdate',
    'afterMultipleUpdate'
];
async function runSingleHook(hooks, orama, id, doc) {
    const hooksLength = hooks.length;
    for(let i = 0; i < hooksLength; i++){
        await hooks[i](orama, id, doc);
    }
}
async function runMultipleHook(hooks, orama, docsOrIds) {
    const hooksLength = hooks.length;
    for(let i = 0; i < hooksLength; i++){
        await hooks[i](orama, docsOrIds);
    }
}
async function runAfterSearch(hooks, db, params, language, results) {
    const hooksLength = hooks.length;
    for(let i = 0; i < hooksLength; i++){
        await hooks[i](db, params, language, results);
    }
}
const BALANCE_STATE = {
    UNBALANCED_RIGHT: -2,
    SLIGHTLY_UNBALANCED_RIGHT: -1,
    BALANCED: 0,
    SLIGHTLY_UNBALANCED_LEFT: 1,
    UNBALANCED_LEFT: 2
};
function getHeight(node) {
    return node != null ? node.h : -1;
}
function rotateLeft(node) {
    const right = node.r;
    node.r = right.l;
    right.l = node;
    node.h = Math.max(getHeight(node.l), getHeight(node.r)) + 1;
    right.h = Math.max(getHeight(right.l), getHeight(right.r)) + 1;
    return right;
}
function rotateRight(node) {
    const left = node.l;
    node.l = left.r;
    left.r = node;
    node.h = Math.max(getHeight(node.l), getHeight(node.r)) + 1;
    left.h = Math.max(getHeight(left.l), getHeight(left.r)) + 1;
    return left;
}
function rangeSearch(node, min, max) {
    if (!node) {
        return [];
    }
    const result = [];
    function traverse(node) {
        if (!node) {
            return;
        }
        if (node.k > min) {
            traverse(node.l);
        }
        if (node.k >= min && node.k <= max) {
            safeArrayPush(result, node.v);
        }
        if (node.k < max) {
            traverse(node.r);
        }
    }
    traverse(node);
    return result;
}
function greaterThan(node, key, inclusive = false) {
    if (!node) {
        return [];
    }
    const result = [];
    function traverse(node) {
        if (!node) {
            return;
        }
        if (inclusive && node.k >= key) {
            safeArrayPush(result, node.v);
        }
        if (!inclusive && node.k > key) {
            safeArrayPush(result, node.v);
        }
        traverse(node.l);
        traverse(node.r);
    }
    traverse(node);
    return result;
}
function lessThan(node, key, inclusive = false) {
    if (!node) {
        return [];
    }
    const result = [];
    function traverse(node) {
        if (!node) {
            return;
        }
        if (inclusive && node.k <= key) {
            safeArrayPush(result, node.v);
        }
        if (!inclusive && node.k < key) {
            safeArrayPush(result, node.v);
        }
        traverse(node.l);
        traverse(node.r);
    }
    traverse(node);
    return result;
}
function getNodeByKey(node, key) {
    while(node !== null){
        if (key < node.k) {
            node = node.l;
        } else if (key > node.k) {
            node = node.r;
        } else {
            return node;
        }
    }
    return null;
}
function create1(key, value) {
    return {
        k: key,
        v: value,
        l: null,
        r: null,
        h: 0
    };
}
function insert(root, key, value) {
    let parent = null;
    let current = root;
    while(current !== null){
        parent = current;
        if (key < current.k) {
            current = current.l;
        } else if (key > current.k) {
            current = current.r;
        } else {
            current.v = current.v.concat(value);
            return root;
        }
    }
    const newNode = create1(key, value);
    if (parent == null) {
        root = newNode;
    } else if (key < parent.k) {
        parent.l = newNode;
    } else {
        parent.r = newNode;
    }
    current = newNode;
    while(parent != null){
        const balanceFactor = getHeight(parent.l) - getHeight(parent.r);
        if (balanceFactor === BALANCE_STATE.UNBALANCED_LEFT) {
            if (key > parent.l.k) {
                parent.l = rotateLeft(parent.l);
            }
            parent = rotateRight(parent);
        }
        if (balanceFactor === BALANCE_STATE.UNBALANCED_RIGHT) {
            if (key < parent.r.k) {
                parent.r = rotateRight(parent.r);
            }
            parent = rotateLeft(parent);
        }
        if (parent === root) {
            break;
        }
        current = parent;
        parent = getNodeParent(root, current.k);
    }
    return root;
}
function getNodeParent(root, key) {
    let current = root;
    let parent = null;
    while(current !== null){
        if (key < current.k) {
            parent = current;
            current = current.l;
        } else if (key > current.k) {
            parent = current;
            current = current.r;
        } else {
            break;
        }
    }
    return parent;
}
function find(root, key) {
    const node = getNodeByKey(root, key);
    if (node == null) {
        return null;
    }
    return node.v;
}
function remove1(root, key) {
    let node = root;
    let parentNode = null;
    while(node != null && node.k !== key){
        parentNode = node;
        if (key < node.k) {
            node = node.l;
        } else {
            node = node.r;
        }
    }
    if (node == null) {
        return null;
    }
    if (node.l == null && node.r == null) {
        if (parentNode == null) {
            root = null;
        } else {
            if (parentNode.l === node) {
                parentNode.l = null;
            } else {
                parentNode.r = null;
            }
        }
    } else if (node.l != null && node.r != null) {
        let minValueNode = node.r;
        let minValueParent = node;
        while(minValueNode.l != null){
            minValueParent = minValueNode;
            minValueNode = minValueNode.l;
        }
        node.k = minValueNode.k;
        if (minValueParent === node) {
            minValueParent.r = minValueNode.r;
        } else {
            minValueParent.l = minValueNode.r;
        }
    } else {
        const childNode = node.l != null ? node.l : node.r;
        if (parentNode == null) {
            root = childNode;
        } else {
            if (parentNode.l === node) {
                parentNode.l = childNode;
            } else {
                parentNode.r = childNode;
            }
        }
    }
    return root;
}
function removeDocument(root, id, key) {
    const node = getNodeByKey(root, key);
    if (!node) {
        return;
    }
    if (node.v.length === 1) {
        remove1(root, key);
        return;
    }
    node.v.splice(node.v.indexOf(id), 1);
}
function create2() {
    return {
        numberToDocumentId: new Map()
    };
}
function insert1(root, key, value) {
    if (root.numberToDocumentId.has(key)) {
        root.numberToDocumentId.get(key).push(value);
        return root;
    }
    root.numberToDocumentId.set(key, [
        value
    ]);
    return root;
}
function removeDocument1(root, id, key) {
    var _root_numberToDocumentId_get, _root_numberToDocumentId_get1;
    root === null || root === void 0 ? void 0 : root.numberToDocumentId.set(key, ((_root_numberToDocumentId_get = root === null || root === void 0 ? void 0 : root.numberToDocumentId.get(key)) === null || _root_numberToDocumentId_get === void 0 ? void 0 : _root_numberToDocumentId_get.filter((v)=>v !== id)) ?? []);
    if (((_root_numberToDocumentId_get1 = root === null || root === void 0 ? void 0 : root.numberToDocumentId.get(key)) === null || _root_numberToDocumentId_get1 === void 0 ? void 0 : _root_numberToDocumentId_get1.length) === 0) {
        root === null || root === void 0 ? void 0 : root.numberToDocumentId.delete(key);
    }
}
function filter(root, operation) {
    const operationKeys = Object.keys(operation);
    if (operationKeys.length !== 1) {
        throw new Error('Invalid operation');
    }
    const operationType = operationKeys[0];
    switch(operationType){
        case 'eq':
            {
                const value = operation[operationType];
                return root.numberToDocumentId.get(value) ?? [];
            }
        case 'in':
            {
                const value = operation[operationType];
                const result = [];
                for (const v of value){
                    const ids = root.numberToDocumentId.get(v);
                    if (ids != null) {
                        safeArrayPush(result, ids);
                    }
                }
                return result;
            }
        case 'nin':
            {
                const value = operation[operationType];
                const result = [];
                const keys = root.numberToDocumentId.keys();
                for (const key of keys){
                    if (value.includes(key)) {
                        continue;
                    }
                    const ids = root.numberToDocumentId.get(key);
                    if (ids != null) {
                        safeArrayPush(result, ids);
                    }
                }
                return result;
            }
    }
    throw new Error('Invalid operation');
}
function filterArr(root, operation) {
    const operationKeys = Object.keys(operation);
    if (operationKeys.length !== 1) {
        throw new Error('Invalid operation');
    }
    const operationType = operationKeys[0];
    switch(operationType){
        case 'containsAll':
            {
                const values = operation[operationType];
                const ids = values.map((value)=>root.numberToDocumentId.get(value) ?? []);
                return intersect(ids);
            }
    }
    throw new Error('Invalid operation');
}
function _boundedLevenshtein(a, b, tolerance) {
    if (a === b) {
        return 0;
    }
    const swap = a;
    if (a.length > b.length) {
        a = b;
        b = swap;
    }
    let lenA = a.length;
    let lenB = b.length;
    while(lenA > 0 && a.charCodeAt(~-lenA) === b.charCodeAt(~-lenB)){
        lenA--;
        lenB--;
    }
    if (!lenA) {
        return lenB > tolerance ? -1 : lenB;
    }
    let startIdx = 0;
    while(startIdx < lenA && a.charCodeAt(startIdx) === b.charCodeAt(startIdx)){
        startIdx++;
    }
    lenA -= startIdx;
    lenB -= startIdx;
    if (lenA === 0) {
        return lenB > tolerance ? -1 : lenB;
    }
    const delta = lenB - lenA;
    if (tolerance > lenB) {
        tolerance = lenB;
    } else if (delta > tolerance) {
        return -1;
    }
    let i = 0;
    const row = [];
    const characterCodeCache = [];
    while(i < tolerance){
        characterCodeCache[i] = b.charCodeAt(startIdx + i);
        row[i] = ++i;
    }
    while(i < lenB){
        characterCodeCache[i] = b.charCodeAt(startIdx + i);
        row[i++] = tolerance + 1;
    }
    const offset = tolerance - delta;
    const haveMax = tolerance < lenB;
    let jStart = 0;
    let jEnd = tolerance;
    let current = 0;
    let left = 0;
    let above = 0;
    let charA = 0;
    let j = 0;
    for(i = 0; i < lenA; i++){
        left = i;
        current = i + 1;
        charA = a.charCodeAt(startIdx + i);
        jStart += i > offset ? 1 : 0;
        jEnd += jEnd < lenB ? 1 : 0;
        for(j = jStart; j < jEnd; j++){
            above = current;
            current = left;
            left = row[j];
            if (charA !== characterCodeCache[j]) {
                if (left < current) {
                    current = left;
                }
                if (above < current) {
                    current = above;
                }
                current++;
            }
            row[j] = current;
        }
        if (haveMax && row[i + delta] > tolerance) {
            return -1;
        }
    }
    return current <= tolerance ? current : -1;
}
function syncBoundedLevenshtein(a, b, tolerance) {
    const distance = _boundedLevenshtein(a, b, tolerance);
    return {
        distance,
        isBounded: distance >= 0
    };
}
class Node {
    constructor(key, subWord, end){
        this.k = key;
        this.s = subWord;
        this.e = end;
    }
    c = {};
    d = [];
    w = '';
    toJSON() {
        return {
            w: this.w,
            s: this.s,
            c: this.c,
            d: this.d,
            e: this.e
        };
    }
}
function updateParent(node, parent) {
    node.w = parent.w + node.s;
}
function addDocument(node, docID) {
    node.d.push(docID);
}
function removeDocument2(node, docID) {
    const index = node.d.indexOf(docID);
    if (index === -1) {
        return false;
    }
    node.d.splice(index, 1);
    return true;
}
function findAllWords(node, output, term, exact, tolerance) {
    if (node.e) {
        const { w, d: docIDs } = node;
        if (exact && w !== term) {
            return {};
        }
        if (getOwnProperty(output, w) == null) {
            if (tolerance) {
                const difference = Math.abs(term.length - w.length);
                if (difference <= tolerance && syncBoundedLevenshtein(term, w, tolerance).isBounded) {
                    output[w] = [];
                }
            } else {
                output[w] = [];
            }
        }
        if (getOwnProperty(output, w) != null && docIDs.length > 0) {
            const docs = new Set(output[w]);
            const docIDsLength = docIDs.length;
            for(let i = 0; i < docIDsLength; i++){
                docs.add(docIDs[i]);
            }
            output[w] = Array.from(docs);
        }
    }
    for (const character of Object.keys(node.c)){
        findAllWords(node.c[character], output, term, exact, tolerance);
    }
    return output;
}
function getCommonPrefix(a, b) {
    let commonPrefix = '';
    const len = Math.min(a.length, b.length);
    for(let i = 0; i < len; i++){
        if (a[i] !== b[i]) {
            return commonPrefix;
        }
        commonPrefix += a[i];
    }
    return commonPrefix;
}
function create3(end = false, subWord = '', key = '') {
    return new Node(key, subWord, end);
}
function insert2(root, word, docId) {
    for(let i = 0; i < word.length; i++){
        const currentCharacter = word[i];
        const wordAtIndex = word.substring(i);
        const rootChildCurrentChar = root.c[currentCharacter];
        if (rootChildCurrentChar) {
            const edgeLabel = rootChildCurrentChar.s;
            const edgeLabelLength = edgeLabel.length;
            const commonPrefix = getCommonPrefix(edgeLabel, wordAtIndex);
            const commonPrefixLength = commonPrefix.length;
            if (edgeLabel === wordAtIndex) {
                addDocument(rootChildCurrentChar, docId);
                rootChildCurrentChar.e = true;
                return;
            }
            const edgeLabelAtCommonPrefix = edgeLabel[commonPrefixLength];
            if (commonPrefixLength < edgeLabelLength && commonPrefixLength === wordAtIndex.length) {
                const newNode = create3(true, wordAtIndex, currentCharacter);
                newNode.c[edgeLabelAtCommonPrefix] = rootChildCurrentChar;
                const newNodeChild = newNode.c[edgeLabelAtCommonPrefix];
                newNodeChild.s = edgeLabel.substring(commonPrefixLength);
                newNodeChild.k = edgeLabelAtCommonPrefix;
                root.c[currentCharacter] = newNode;
                updateParent(newNode, root);
                updateParent(newNodeChild, newNode);
                addDocument(newNode, docId);
                return;
            }
            if (commonPrefixLength < edgeLabelLength && commonPrefixLength < wordAtIndex.length) {
                const inbetweenNode = create3(false, commonPrefix, currentCharacter);
                inbetweenNode.c[edgeLabelAtCommonPrefix] = rootChildCurrentChar;
                root.c[currentCharacter] = inbetweenNode;
                const inbetweenNodeChild = inbetweenNode.c[edgeLabelAtCommonPrefix];
                inbetweenNodeChild.s = edgeLabel.substring(commonPrefixLength);
                inbetweenNodeChild.k = edgeLabelAtCommonPrefix;
                const wordAtCommonPrefix = wordAtIndex[commonPrefixLength];
                const newNode = create3(true, word.substring(i + commonPrefixLength), wordAtCommonPrefix);
                addDocument(newNode, docId);
                inbetweenNode.c[wordAtCommonPrefix] = newNode;
                updateParent(inbetweenNode, root);
                updateParent(newNode, inbetweenNode);
                updateParent(inbetweenNodeChild, inbetweenNode);
                return;
            }
            i += edgeLabelLength - 1;
            root = rootChildCurrentChar;
        } else {
            const newNode = create3(true, wordAtIndex, currentCharacter);
            addDocument(newNode, docId);
            root.c[currentCharacter] = newNode;
            updateParent(newNode, root);
            return;
        }
    }
}
function _findLevenshtein(node, term, index, tolerance, originalTolerance, output) {
    if (tolerance < 0) {
        return;
    }
    if (node.e) {
        const { w, d: docIDs } = node;
        if (w) {
            const difference = Math.abs(term.length - w.length);
            if (difference <= originalTolerance && syncBoundedLevenshtein(term, w, originalTolerance).isBounded) {
                output[w] = [];
            }
            if (getOwnProperty(output, w) != null && docIDs.length > 0) {
                const docs = new Set(output[w]);
                const docIDsLength = docIDs.length;
                for(let i = 0; i < docIDsLength; i++){
                    docs.add(docIDs[i]);
                }
                output[w] = Array.from(docs);
            }
        }
    }
    if (index >= term.length) {
        return;
    }
    if (term[index] in node.c) {
        _findLevenshtein(node.c[term[index]], term, index + 1, tolerance, originalTolerance, output);
    }
    _findLevenshtein(node, term, index + 1, tolerance - 1, originalTolerance, output);
    for(const character in node.c){
        _findLevenshtein(node.c[character], term, index, tolerance - 1, originalTolerance, output);
    }
    for(const character in node.c){
        if (character !== term[index]) {
            _findLevenshtein(node.c[character], term, index + 1, tolerance - 1, originalTolerance, output);
        }
    }
}
function find1(root, { term, exact, tolerance }) {
    if (tolerance && !exact) {
        const output = {};
        tolerance = tolerance || 0;
        _findLevenshtein(root, term, 0, tolerance || 0, tolerance, output);
        return output;
    } else {
        for(let i = 0; i < term.length; i++){
            const character = term[i];
            if (character in root.c) {
                const rootChildCurrentChar = root.c[character];
                const edgeLabel = rootChildCurrentChar.s;
                const termSubstring = term.substring(i);
                const commonPrefix = getCommonPrefix(edgeLabel, termSubstring);
                const commonPrefixLength = commonPrefix.length;
                if (commonPrefixLength !== edgeLabel.length && commonPrefixLength !== termSubstring.length) {
                    if (tolerance) break;
                    return {};
                }
                i += rootChildCurrentChar.s.length - 1;
                root = rootChildCurrentChar;
            } else {
                return {};
            }
        }
        const output = {};
        findAllWords(root, output, term, exact, tolerance);
        return output;
    }
}
function removeDocumentByWord(root, term, docID, exact = true) {
    if (!term) {
        return true;
    }
    for(let i = 0; i < term.length; i++){
        const character = term[i];
        if (character in root.c) {
            const rootChildCurrentChar = root.c[character];
            i += rootChildCurrentChar.s.length - 1;
            root = rootChildCurrentChar;
            if (exact && root.w !== term) {} else {
                removeDocument2(root, docID);
            }
        } else {
            return false;
        }
    }
    return true;
}
function create4() {
    return {
        root: null
    };
}
function insert3(tree, point, docIDs) {
    const newNode = {
        point,
        docIDs
    };
    if (tree.root == null) {
        tree.root = newNode;
        return;
    }
    let node = tree.root;
    let depth = 0;
    while(node !== null){
        if (node.point.lon === point.lon && node.point.lat === point.lat) {
            const newDocIDs = node.docIDs ?? [];
            node.docIDs = Array.from(new Set([
                ...newDocIDs,
                ...docIDs || []
            ]));
            return;
        }
        const axis = depth % 2;
        if (axis === 0) {
            if (point.lon < node.point.lon) {
                if (node.left == null) {
                    node.left = newNode;
                    return;
                }
                node = node.left;
            } else {
                if (node.right == null) {
                    node.right = newNode;
                    return;
                }
                node = node.right;
            }
        } else {
            if (point.lat < node.point.lat) {
                if (node.left == null) {
                    node.left = newNode;
                    return;
                }
                node = node.left;
            } else {
                if (node.right == null) {
                    node.right = newNode;
                    return;
                }
                node = node.right;
            }
        }
        depth++;
    }
}
function removeDocByID(tree, point, docID) {
    let node = tree.root;
    let depth = 0;
    let parentNode = null;
    let direction = null;
    while(node !== null){
        if ((node === null || node === void 0 ? void 0 : node.point.lon) === point.lon && node.point.lat === point.lat) {
            var _node_docIDs;
            const index = (_node_docIDs = node.docIDs) === null || _node_docIDs === void 0 ? void 0 : _node_docIDs.indexOf(docID);
            if (index !== undefined && index > -1) {
                var _node_docIDs1;
                (_node_docIDs1 = node.docIDs) === null || _node_docIDs1 === void 0 ? void 0 : _node_docIDs1.splice(index, 1);
                if (node.docIDs == null || node.docIDs.length === 0) {
                    if (parentNode != null) {
                        if (direction === 'left') {
                            parentNode.left = node.left !== null ? node.left : node.right;
                        } else if (direction === 'right') {
                            parentNode.right = node.right !== null ? node.right : node.left;
                        }
                    } else {
                        tree.root = node.left !== null ? node.left : node.right;
                    }
                }
                return;
            }
        }
        const axis = depth % 2;
        parentNode = node;
        if (axis === 0) {
            if (point.lon < node.point.lon) {
                node = node === null || node === void 0 ? void 0 : node.left;
                direction = 'left';
            } else {
                node = node === null || node === void 0 ? void 0 : node.right;
                direction = 'right';
            }
        } else {
            if (point.lat < node.point.lat) {
                node = node === null || node === void 0 ? void 0 : node.left;
                direction = 'left';
            } else {
                node = node === null || node === void 0 ? void 0 : node.right;
                direction = 'right';
            }
        }
        depth++;
    }
}
function searchByRadius(node, center, radius, inclusive = true, sort = 'asc', highPrecision = false) {
    const distanceFn = highPrecision ? vincentyDistance : haversineDistance;
    const stack = [
        {
            node,
            depth: 0
        }
    ];
    const result = [];
    while(stack.length > 0){
        const { node, depth } = stack.pop();
        if (node === null) continue;
        const dist = distanceFn(center, node.point);
        if (inclusive ? dist <= radius : dist > radius) {
            result.push({
                point: node.point,
                docIDs: node.docIDs ?? []
            });
        }
        if (node.left != null) {
            stack.push({
                node: node.left,
                depth: depth + 1
            });
        }
        if (node.right != null) {
            stack.push({
                node: node.right,
                depth: depth + 1
            });
        }
    }
    if (sort) {
        result.sort((a, b)=>{
            const distA = distanceFn(center, a.point);
            const distB = distanceFn(center, b.point);
            return sort.toLowerCase() === 'asc' ? distA - distB : distB - distA;
        });
    }
    return result;
}
function searchByPolygon(root, polygon, inclusive = true, sort = null, highPrecision = false) {
    const stack = [
        {
            node: root,
            depth: 0
        }
    ];
    const result = [];
    while(stack.length > 0){
        const task = stack.pop();
        if (task == null || task.node == null) continue;
        const { node, depth } = task;
        const nextDepth = depth + 1;
        if (node.left != null) {
            stack.push({
                node: node.left,
                depth: nextDepth
            });
        }
        if (node.right != null) {
            stack.push({
                node: node.right,
                depth: nextDepth
            });
        }
        const isInsidePolygon = isPointInPolygon(polygon, node.point);
        if (isInsidePolygon && inclusive) {
            result.push({
                point: node.point,
                docIDs: node.docIDs ?? []
            });
        } else if (!isInsidePolygon && !inclusive) {
            result.push({
                point: node.point,
                docIDs: node.docIDs ?? []
            });
        }
    }
    const centroid = calculatePolygonCentroid(polygon);
    if (sort) {
        const sortFn = highPrecision ? vincentyDistance : haversineDistance;
        result.sort((a, b)=>{
            const distA = sortFn(centroid, a.point);
            const distB = sortFn(centroid, b.point);
            return sort.toLowerCase() === 'asc' ? distA - distB : distB - distA;
        });
    }
    return result;
}
function calculatePolygonCentroid(polygon) {
    let totalArea = 0;
    let centroidX = 0;
    let centroidY = 0;
    for(let i = 0, j = polygon.length - 1; i < polygon.length; j = i++){
        const xi = polygon[i].lon;
        const yi = polygon[i].lat;
        const xj = polygon[j].lon;
        const yj = polygon[j].lat;
        const areaSegment = xi * yj - xj * yi;
        totalArea += areaSegment;
        centroidX += (xi + xj) * areaSegment;
        centroidY += (yi + yj) * areaSegment;
    }
    totalArea /= 2;
    centroidX /= 6 * totalArea;
    centroidY /= 6 * totalArea;
    return {
        lon: centroidX,
        lat: centroidY
    };
}
function isPointInPolygon(polygon, point) {
    let isInside = false;
    const x = point.lon;
    const y = point.lat;
    for(let i = 0, j = polygon.length - 1; i < polygon.length; j = i++){
        const xi = polygon[i].lon;
        const yi = polygon[i].lat;
        const xj = polygon[j].lon;
        const yj = polygon[j].lat;
        const intersect = yi > y !== yj > y && x < (xj - xi) * (y - yi) / (yj - yi) + xi;
        if (intersect) isInside = !isInside;
    }
    return isInside;
}
function haversineDistance(coord1, coord2) {
    const P = Math.PI / 180;
    const lat1 = coord1.lat * P;
    const lat2 = coord2.lat * P;
    const deltaLat = (coord2.lat - coord1.lat) * P;
    const deltaLon = (coord2.lon - coord1.lon) * P;
    const a = Math.sin(deltaLat / 2) * Math.sin(deltaLat / 2) + Math.cos(lat1) * Math.cos(lat2) * Math.sin(deltaLon / 2) * Math.sin(deltaLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return 6371e3 * c;
}
function vincentyDistance(coord1, coord2) {
    const f = 1 / 298.257223563;
    const b = (1 - f) * 6378137;
    const P = Math.PI / 180;
    const lat1 = coord1.lat * P;
    const lat2 = coord2.lat * P;
    const deltaLon = (coord2.lon - coord1.lon) * P;
    const U1 = Math.atan((1 - f) * Math.tan(lat1));
    const U2 = Math.atan((1 - f) * Math.tan(lat2));
    const sinU1 = Math.sin(U1);
    const cosU1 = Math.cos(U1);
    const sinU2 = Math.sin(U2);
    const cosU2 = Math.cos(U2);
    let lambda = deltaLon;
    let prevLambda;
    let iterationLimit = 1000;
    let sinAlpha;
    let cos2Alpha;
    let sinSigma;
    let cosSigma;
    let sigma;
    do {
        const sinLambda = Math.sin(lambda);
        const cosLambda = Math.cos(lambda);
        sinSigma = Math.sqrt(cosU2 * sinLambda * (cosU2 * sinLambda) + (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda));
        cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosLambda;
        sigma = Math.atan2(sinSigma, cosSigma);
        sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma;
        cos2Alpha = 1 - sinAlpha * sinAlpha;
        const cos2SigmaM = cosSigma - 2 * sinU1 * sinU2 / cos2Alpha;
        const C = f / 16 * cos2Alpha * (4 + f * (4 - 3 * cos2Alpha));
        prevLambda = lambda;
        lambda = deltaLon + (1 - C) * f * sinAlpha * (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)));
    }while (Math.abs(lambda - prevLambda) > 1e-12 && --iterationLimit > 0)
    const u2 = cos2Alpha * (6378137 * 6378137 - b * b) / (b * b);
    const A = 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)));
    const B = u2 / 1024 * (256 + u2 * (-128 + u2 * (74 - 47 * u2)));
    const deltaSigma = B * sinSigma * (cosSigma - 2 * sinU1 * sinU2 / cos2Alpha + B / 4 * (cosSigma * (-1 + 2 * sinSigma * sinSigma) - B / 6 * sigma * (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * sigma * sigma)));
    const s = b * A * (sigma - deltaSigma);
    return s;
}
function prioritizeTokenScores(arrays, boost, threshold = 1, keywordsCount) {
    if (boost === 0) {
        throw createError('INVALID_BOOST_VALUE');
    }
    const tokenScoresMap = new Map();
    const tokenKeywordsCountMap = new Map();
    const mapsLength = arrays.length;
    for(let i = 0; i < mapsLength; i++){
        const arr = arrays[i];
        const entriesLength = arr.length;
        for(let j = 0; j < entriesLength; j++){
            const [token, score] = arr[j];
            const boostScore = score * boost;
            const oldScore = tokenScoresMap.get(token);
            if (oldScore !== undefined) {
                tokenScoresMap.set(token, oldScore * 1.5 + boostScore);
                tokenKeywordsCountMap.set(token, tokenKeywordsCountMap.get(token) + 1);
            } else {
                tokenScoresMap.set(token, boostScore);
                tokenKeywordsCountMap.set(token, 1);
            }
        }
    }
    const tokenScores = [];
    for (const tokenScoreEntry of tokenScoresMap.entries()){
        tokenScores.push(tokenScoreEntry);
    }
    const results = tokenScores.sort((a, b)=>b[1] - a[1]);
    if (threshold === 1) {
        return results;
    }
    const allResults = results.length;
    const tokenKeywordsCount = [];
    for (const tokenKeywordsCountEntry of tokenKeywordsCountMap.entries()){
        tokenKeywordsCount.push(tokenKeywordsCountEntry);
    }
    const keywordsPerToken = tokenKeywordsCount.sort((a, b)=>b[1] - a[1]);
    let lastTokenWithAllKeywords = undefined;
    for(let i = 0; i < allResults; i++){
        if (keywordsPerToken[i][1] === keywordsCount) {
            lastTokenWithAllKeywords = i;
        } else {
            break;
        }
    }
    if (typeof lastTokenWithAllKeywords === 'undefined') {
        if (threshold === 0) {
            return [];
        }
        lastTokenWithAllKeywords = 0;
    }
    if (threshold === 0) {
        return results.slice(0, lastTokenWithAllKeywords + 1);
    }
    const thresholdLength = lastTokenWithAllKeywords + Math.ceil(threshold * 100 * (results.length - lastTokenWithAllKeywords) / 100);
    return results.slice(0, results.length + thresholdLength);
}
function BM25(tf, matchingCount, docsCount, fieldLength, averageFieldLength, BM25Params) {
    const { k, b, d } = BM25Params;
    const idf = Math.log(1 + (docsCount - matchingCount + 0.5) / (matchingCount + 0.5));
    return idf * (d + tf * (k + 1)) / (tf + k * (1 - b + b * fieldLength / averageFieldLength));
}
function getMagnitude(vector, vectorLength) {
    let magnitude = 0;
    for(let i = 0; i < vectorLength; i++){
        magnitude += vector[i] * vector[i];
    }
    return Math.sqrt(magnitude);
}
async function insertDocumentScoreParameters(index, prop, id, tokens, docsCount) {
    const internalId = getInternalDocumentId(index.sharedInternalDocumentStore, id);
    index.avgFieldLength[prop] = ((index.avgFieldLength[prop] ?? 0) * (docsCount - 1) + tokens.length) / docsCount;
    index.fieldLengths[prop][internalId] = tokens.length;
    index.frequencies[prop][internalId] = {};
}
async function insertTokenScoreParameters(index, prop, id, tokens, token) {
    let tokenFrequency = 0;
    for (const t of tokens){
        if (t === token) {
            tokenFrequency++;
        }
    }
    const internalId = getInternalDocumentId(index.sharedInternalDocumentStore, id);
    const tf = tokenFrequency / tokens.length;
    index.frequencies[prop][internalId][token] = tf;
    if (!(token in index.tokenOccurrences[prop])) {
        index.tokenOccurrences[prop][token] = 0;
    }
    index.tokenOccurrences[prop][token] = (index.tokenOccurrences[prop][token] ?? 0) + 1;
}
async function removeDocumentScoreParameters(index, prop, id, docsCount) {
    const internalId = getInternalDocumentId(index.sharedInternalDocumentStore, id);
    index.avgFieldLength[prop] = (index.avgFieldLength[prop] * docsCount - index.fieldLengths[prop][internalId]) / (docsCount - 1);
    index.fieldLengths[prop][internalId] = undefined;
    index.frequencies[prop][internalId] = undefined;
}
async function removeTokenScoreParameters(index, prop, token) {
    index.tokenOccurrences[prop][token]--;
}
async function calculateResultScores(context, index, prop, term, ids) {
    const documentIDs = Array.from(ids);
    const avgFieldLength = index.avgFieldLength[prop];
    const fieldLengths = index.fieldLengths[prop];
    const oramaOccurrences = index.tokenOccurrences[prop];
    const oramaFrequencies = index.frequencies[prop];
    const termOccurrences = typeof oramaOccurrences[term] === 'number' ? oramaOccurrences[term] ?? 0 : 0;
    const scoreList = [];
    const documentIDsLength = documentIDs.length;
    for(let k = 0; k < documentIDsLength; k++){
        var _oramaFrequencies_internalId;
        const internalId = getInternalDocumentId(index.sharedInternalDocumentStore, documentIDs[k]);
        const tf = (oramaFrequencies === null || oramaFrequencies === void 0 ? void 0 : (_oramaFrequencies_internalId = oramaFrequencies[internalId]) === null || _oramaFrequencies_internalId === void 0 ? void 0 : _oramaFrequencies_internalId[term]) ?? 0;
        const bm25 = BM25(tf, termOccurrences, context.docsCount, fieldLengths[internalId], avgFieldLength, context.params.relevance);
        scoreList.push([
            internalId,
            bm25
        ]);
    }
    return scoreList;
}
async function create5(orama, sharedInternalDocumentStore, schema, index, prefix = '') {
    if (!index) {
        index = {
            sharedInternalDocumentStore,
            indexes: {},
            vectorIndexes: {},
            searchableProperties: [],
            searchablePropertiesWithTypes: {},
            frequencies: {},
            tokenOccurrences: {},
            avgFieldLength: {},
            fieldLengths: {}
        };
    }
    for (const [prop, type] of Object.entries(schema)){
        const path = `${prefix}${prefix ? '.' : ''}${prop}`;
        if (typeof type === 'object' && !Array.isArray(type)) {
            create5(orama, sharedInternalDocumentStore, type, index, path);
            continue;
        }
        if (isVectorType(type)) {
            index.searchableProperties.push(path);
            index.searchablePropertiesWithTypes[path] = type;
            index.vectorIndexes[path] = {
                size: getVectorSize(type),
                vectors: {}
            };
        } else {
            const isArray = /\[/.test(type);
            switch(type){
                case 'boolean':
                case 'boolean[]':
                    index.indexes[path] = {
                        type: 'Bool',
                        node: {
                            true: [],
                            false: []
                        },
                        isArray
                    };
                    break;
                case 'number':
                case 'number[]':
                    index.indexes[path] = {
                        type: 'AVL',
                        node: create1(0, []),
                        isArray
                    };
                    break;
                case 'string':
                case 'string[]':
                    index.indexes[path] = {
                        type: 'Radix',
                        node: create3(),
                        isArray
                    };
                    index.avgFieldLength[path] = 0;
                    index.frequencies[path] = {};
                    index.tokenOccurrences[path] = {};
                    index.fieldLengths[path] = {};
                    break;
                case 'enum':
                case 'enum[]':
                    index.indexes[path] = {
                        type: 'Flat',
                        node: create2(),
                        isArray
                    };
                    break;
                case 'geopoint':
                    index.indexes[path] = {
                        type: 'BKD',
                        node: create4(),
                        isArray
                    };
                    break;
                default:
                    throw createError('INVALID_SCHEMA_TYPE', Array.isArray(type) ? 'array' : type, path);
            }
            index.searchableProperties.push(path);
            index.searchablePropertiesWithTypes[path] = type;
        }
    }
    return index;
}
async function insertScalar(implementation, index, prop, id, value, schemaType, language, tokenizer, docsCount) {
    const internalId = getInternalDocumentId(index.sharedInternalDocumentStore, id);
    const { type, node } = index.indexes[prop];
    switch(type){
        case 'Bool':
            {
                node[value ? 'true' : 'false'].push(internalId);
                break;
            }
        case 'AVL':
            {
                insert(node, value, [
                    internalId
                ]);
                break;
            }
        case 'Radix':
            {
                const tokens = await tokenizer.tokenize(value, language, prop);
                await implementation.insertDocumentScoreParameters(index, prop, internalId, tokens, docsCount);
                for (const token of tokens){
                    await implementation.insertTokenScoreParameters(index, prop, internalId, tokens, token);
                    insert2(node, token, internalId);
                }
                break;
            }
        case 'Flat':
            {
                insert1(node, value, internalId);
                break;
            }
        case 'BKD':
            {
                insert3(node, value, [
                    internalId
                ]);
                break;
            }
    }
}
async function insert4(implementation, index, prop, id, value, schemaType, language, tokenizer, docsCount) {
    if (isVectorType(schemaType)) {
        return insertVector(index, prop, value, id);
    }
    if (!isArrayType(schemaType)) {
        return insertScalar(implementation, index, prop, id, value, schemaType, language, tokenizer, docsCount);
    }
    const innerSchemaType = getInnerType(schemaType);
    const elements = value;
    const elementsLength = elements.length;
    for(let i = 0; i < elementsLength; i++){
        await insertScalar(implementation, index, prop, id, elements[i], innerSchemaType, language, tokenizer, docsCount);
    }
}
function insertVector(index, prop, value, id) {
    if (!(value instanceof Float32Array)) {
        value = new Float32Array(value);
    }
    const size = index.vectorIndexes[prop].size;
    const magnitude = getMagnitude(value, size);
    index.vectorIndexes[prop].vectors[id] = [
        magnitude,
        value
    ];
}
async function removeScalar(implementation, index, prop, id, value, schemaType, language, tokenizer, docsCount) {
    const internalId = getInternalDocumentId(index.sharedInternalDocumentStore, id);
    if (isVectorType(schemaType)) {
        delete index.vectorIndexes[prop].vectors[id];
        return true;
    }
    const { type, node } = index.indexes[prop];
    switch(type){
        case 'AVL':
            {
                removeDocument(node, internalId, value);
                return true;
            }
        case 'Bool':
            {
                const booleanKey = value ? 'true' : 'false';
                const position = node[booleanKey].indexOf(internalId);
                node[value ? 'true' : 'false'].splice(position, 1);
                return true;
            }
        case 'Radix':
            {
                const tokens = await tokenizer.tokenize(value, language, prop);
                await implementation.removeDocumentScoreParameters(index, prop, id, docsCount);
                for (const token of tokens){
                    await implementation.removeTokenScoreParameters(index, prop, token);
                    removeDocumentByWord(node, token, internalId);
                }
                return true;
            }
        case 'Flat':
            {
                removeDocument1(node, internalId, value);
                return true;
            }
        case 'BKD':
            {
                removeDocByID(node, value, internalId);
                return false;
            }
    }
}
async function remove2(implementation, index, prop, id, value, schemaType, language, tokenizer, docsCount) {
    if (!isArrayType(schemaType)) {
        return removeScalar(implementation, index, prop, id, value, schemaType, language, tokenizer, docsCount);
    }
    const innerSchemaType = getInnerType(schemaType);
    const elements = value;
    const elementsLength = elements.length;
    for(let i = 0; i < elementsLength; i++){
        await removeScalar(implementation, index, prop, id, elements[i], innerSchemaType, language, tokenizer, docsCount);
    }
    return true;
}
async function search(context, index, prop, term) {
    if (!(prop in index.tokenOccurrences)) {
        return [];
    }
    const { node, type } = index.indexes[prop];
    if (type !== 'Radix') {
        throw createError('WRONG_SEARCH_PROPERTY_TYPE', prop);
    }
    const { exact, tolerance } = context.params;
    const searchResult = find1(node, {
        term,
        exact,
        tolerance
    });
    const ids = new Set();
    for(const key in searchResult){
        for (const id of searchResult[key]){
            ids.add(id);
        }
    }
    return context.index.calculateResultScores(context, index, prop, term, Array.from(ids));
}
async function searchByWhereClause(context, index, filters) {
    const filterKeys = Object.keys(filters);
    const filtersMap = filterKeys.reduce((acc, key)=>({
            [key]: [],
            ...acc
        }), {});
    for (const param of filterKeys){
        const operation = filters[param];
        if (typeof index.indexes[param] === 'undefined') {
            throw createError('UNKNOWN_FILTER_PROPERTY', param);
        }
        const { node, type, isArray } = index.indexes[param];
        if (type === 'Bool') {
            const idx = node;
            const filteredIDs = idx[operation.toString()];
            safeArrayPush(filtersMap[param], filteredIDs);
            continue;
        }
        if (type === 'BKD') {
            let reqOperation;
            if ('radius' in operation) {
                reqOperation = 'radius';
            } else if ('polygon' in operation) {
                reqOperation = 'polygon';
            } else {
                throw new Error(`Invalid operation ${operation}`);
            }
            if (reqOperation === 'radius') {
                const { value, coordinates, unit = 'm', inside = true, highPrecision = false } = operation[reqOperation];
                const distanceInMeters = convertDistanceToMeters(value, unit);
                const ids = searchByRadius(node.root, coordinates, distanceInMeters, inside, undefined, highPrecision);
                safeArrayPush(filtersMap[param], ids.map(({ docIDs })=>docIDs).flat());
            } else {
                const { coordinates, inside = true, highPrecision = false } = operation[reqOperation];
                const ids = searchByPolygon(node.root, coordinates, inside, undefined, highPrecision);
                safeArrayPush(filtersMap[param], ids.map(({ docIDs })=>docIDs).flat());
            }
            continue;
        }
        if (type === 'Radix' && (typeof operation === 'string' || Array.isArray(operation))) {
            for (const raw of [
                operation
            ].flat()){
                const term = await context.tokenizer.tokenize(raw, context.language, param);
                for (const t of term){
                    const filteredIDsResults = find1(node, {
                        term: t,
                        exact: true
                    });
                    safeArrayPush(filtersMap[param], Object.values(filteredIDsResults).flat());
                }
            }
            continue;
        }
        const operationKeys = Object.keys(operation);
        if (operationKeys.length > 1) {
            throw createError('INVALID_FILTER_OPERATION', operationKeys.length);
        }
        if (type === 'Flat') {
            if (isArray) {
                safeArrayPush(filtersMap[param], filterArr(node, operation));
            } else {
                safeArrayPush(filtersMap[param], filter(node, operation));
            }
            continue;
        }
        if (type === 'AVL') {
            const operationOpt = operationKeys[0];
            const operationValue = operation[operationOpt];
            let filteredIDs = [];
            switch(operationOpt){
                case 'gt':
                    {
                        filteredIDs = greaterThan(node, operationValue, false);
                        break;
                    }
                case 'gte':
                    {
                        filteredIDs = greaterThan(node, operationValue, true);
                        break;
                    }
                case 'lt':
                    {
                        filteredIDs = lessThan(node, operationValue, false);
                        break;
                    }
                case 'lte':
                    {
                        filteredIDs = lessThan(node, operationValue, true);
                        break;
                    }
                case 'eq':
                    {
                        filteredIDs = find(node, operationValue) ?? [];
                        break;
                    }
                case 'between':
                    {
                        const [min, max] = operationValue;
                        filteredIDs = rangeSearch(node, min, max);
                        break;
                    }
            }
            safeArrayPush(filtersMap[param], filteredIDs);
        }
    }
    const result = intersect(Object.values(filtersMap));
    return result;
}
async function getSearchableProperties(index) {
    return index.searchableProperties;
}
async function getSearchablePropertiesWithTypes(index) {
    return index.searchablePropertiesWithTypes;
}
function loadRadixNode(node) {
    const convertedNode = create3(node.e, node.s, node.k);
    convertedNode.d = node.d;
    convertedNode.w = node.w;
    for (const childrenKey of Object.keys(node.c)){
        convertedNode.c[childrenKey] = loadRadixNode(node.c[childrenKey]);
    }
    return convertedNode;
}
function loadFlatNode(node) {
    return {
        numberToDocumentId: new Map(node)
    };
}
function saveFlatNode(node) {
    return Array.from(node.numberToDocumentId.entries());
}
async function load2(sharedInternalDocumentStore, raw) {
    const { indexes: rawIndexes, vectorIndexes: rawVectorIndexes, searchableProperties, searchablePropertiesWithTypes, frequencies, tokenOccurrences, avgFieldLength, fieldLengths } = raw;
    const indexes = {};
    const vectorIndexes = {};
    for (const prop of Object.keys(rawIndexes)){
        const { node, type, isArray } = rawIndexes[prop];
        switch(type){
            case 'Radix':
                indexes[prop] = {
                    type: 'Radix',
                    node: loadRadixNode(node),
                    isArray
                };
                break;
            case 'Flat':
                indexes[prop] = {
                    type: 'Flat',
                    node: loadFlatNode(node),
                    isArray
                };
                break;
            default:
                indexes[prop] = rawIndexes[prop];
        }
    }
    for (const idx of Object.keys(rawVectorIndexes)){
        const vectors = rawVectorIndexes[idx].vectors;
        for(const vec in vectors){
            vectors[vec] = [
                vectors[vec][0],
                new Float32Array(vectors[vec][1])
            ];
        }
        vectorIndexes[idx] = {
            size: rawVectorIndexes[idx].size,
            vectors
        };
    }
    return {
        sharedInternalDocumentStore,
        indexes,
        vectorIndexes,
        searchableProperties,
        searchablePropertiesWithTypes,
        frequencies,
        tokenOccurrences,
        avgFieldLength,
        fieldLengths
    };
}
async function save2(index) {
    const { indexes, vectorIndexes, searchableProperties, searchablePropertiesWithTypes, frequencies, tokenOccurrences, avgFieldLength, fieldLengths } = index;
    const vectorIndexesAsArrays = {};
    for (const idx of Object.keys(vectorIndexes)){
        const vectors = vectorIndexes[idx].vectors;
        for(const vec in vectors){
            vectors[vec] = [
                vectors[vec][0],
                Array.from(vectors[vec][1])
            ];
        }
        vectorIndexesAsArrays[idx] = {
            size: vectorIndexes[idx].size,
            vectors
        };
    }
    const savedIndexes = {};
    for (const name of Object.keys(indexes)){
        const { type, node, isArray } = indexes[name];
        if (type !== 'Flat') {
            savedIndexes[name] = indexes[name];
            continue;
        }
        savedIndexes[name] = {
            type: 'Flat',
            node: saveFlatNode(node),
            isArray
        };
    }
    return {
        indexes: savedIndexes,
        vectorIndexes: vectorIndexesAsArrays,
        searchableProperties,
        searchablePropertiesWithTypes,
        frequencies,
        tokenOccurrences,
        avgFieldLength,
        fieldLengths
    };
}
async function createIndex() {
    return {
        create: create5,
        insert: insert4,
        remove: remove2,
        insertDocumentScoreParameters,
        insertTokenScoreParameters,
        removeDocumentScoreParameters,
        removeTokenScoreParameters,
        calculateResultScores,
        search,
        searchByWhereClause,
        getSearchableProperties,
        getSearchablePropertiesWithTypes,
        load: load2,
        save: save2
    };
}
function innerCreate(orama, sharedInternalDocumentStore, schema, sortableDeniedProperties, prefix) {
    const sorter = {
        language: orama.tokenizer.language,
        sharedInternalDocumentStore,
        enabled: true,
        isSorted: true,
        sortableProperties: [],
        sortablePropertiesWithTypes: {},
        sorts: {}
    };
    for (const [prop, type] of Object.entries(schema)){
        const path = `${prefix}${prefix ? '.' : ''}${prop}`;
        if (sortableDeniedProperties.includes(path)) {
            continue;
        }
        if (typeof type === 'object' && !Array.isArray(type)) {
            const ret = innerCreate(orama, sharedInternalDocumentStore, type, sortableDeniedProperties, path);
            safeArrayPush(sorter.sortableProperties, ret.sortableProperties);
            sorter.sorts = {
                ...sorter.sorts,
                ...ret.sorts
            };
            sorter.sortablePropertiesWithTypes = {
                ...sorter.sortablePropertiesWithTypes,
                ...ret.sortablePropertiesWithTypes
            };
            continue;
        }
        if (!isVectorType(type)) {
            switch(type){
                case 'boolean':
                case 'number':
                case 'string':
                    sorter.sortableProperties.push(path);
                    sorter.sortablePropertiesWithTypes[path] = type;
                    sorter.sorts[path] = {
                        docs: new Map(),
                        orderedDocsToRemove: new Map(),
                        orderedDocs: [],
                        type: type
                    };
                    break;
                case 'geopoint':
                case 'enum':
                    continue;
                case 'enum[]':
                case 'boolean[]':
                case 'number[]':
                case 'string[]':
                    continue;
                default:
                    throw createError('INVALID_SORT_SCHEMA_TYPE', Array.isArray(type) ? 'array' : type, path);
            }
        }
    }
    return sorter;
}
async function create6(orama, sharedInternalDocumentStore, schema, config) {
    const isSortEnabled = (config === null || config === void 0 ? void 0 : config.enabled) !== false;
    if (!isSortEnabled) {
        return {
            disabled: true
        };
    }
    return innerCreate(orama, sharedInternalDocumentStore, schema, (config || {}).unsortableProperties || [], '');
}
async function insert5(sorter, prop, id, value) {
    if (!sorter.enabled) {
        return;
    }
    sorter.isSorted = false;
    const internalId = getInternalDocumentId(sorter.sharedInternalDocumentStore, id);
    const s = sorter.sorts[prop];
    s.docs.set(internalId, s.orderedDocs.length);
    s.orderedDocs.push([
        internalId,
        value
    ]);
}
function ensureIsSorted(sorter) {
    if (sorter.isSorted) {
        return;
    }
    if (!sorter.enabled) {
        return;
    }
    const properties = Object.keys(sorter.sorts);
    for (const prop of properties){
        ensurePropertyIsSorted(sorter, prop);
    }
    sorter.isSorted = true;
}
function stringSort(language, value, d) {
    return value[1].localeCompare(d[1], language);
}
function numberSort(value, d) {
    return value[1] - d[1];
}
function booleanSort(value, d) {
    return d[1] ? -1 : 1;
}
function ensurePropertyIsSorted(sorter, prop) {
    const s = sorter.sorts[prop];
    let predicate;
    switch(s.type){
        case 'string':
            predicate = stringSort.bind(null, sorter.language);
            break;
        case 'number':
            predicate = numberSort.bind(null);
            break;
        case 'boolean':
            predicate = booleanSort.bind(null);
            break;
    }
    s.orderedDocs.sort(predicate);
    const orderedDocsLength = s.orderedDocs.length;
    for(let i = 0; i < orderedDocsLength; i++){
        const docId = s.orderedDocs[i][0];
        s.docs.set(docId, i);
    }
}
function ensureOrderedDocsAreDeleted(sorter) {
    const properties = Object.keys(sorter.sorts);
    for (const prop of properties){
        ensureOrderedDocsAreDeletedByProperty(sorter, prop);
    }
}
function ensureOrderedDocsAreDeletedByProperty(sorter, prop) {
    const s = sorter.sorts[prop];
    if (!s.orderedDocsToRemove.size) return;
    s.orderedDocs = s.orderedDocs.filter((doc)=>!s.orderedDocsToRemove.has(doc[0]));
    s.orderedDocsToRemove.clear();
}
async function remove3(sorter, prop, id) {
    if (!sorter.enabled) {
        return;
    }
    const s = sorter.sorts[prop];
    const internalId = getInternalDocumentId(sorter.sharedInternalDocumentStore, id);
    const index = s.docs.get(internalId);
    if (!index) return;
    s.docs.delete(internalId);
    s.orderedDocsToRemove.set(internalId, true);
}
async function sortBy(sorter, docIds, by) {
    if (!sorter.enabled) {
        throw createError('SORT_DISABLED');
    }
    const property = by.property;
    const isDesc = by.order === 'DESC';
    const s = sorter.sorts[property];
    if (!s) {
        throw createError('UNABLE_TO_SORT_ON_UNKNOWN_FIELD', property, sorter.sortableProperties.join(', '));
    }
    ensureOrderedDocsAreDeletedByProperty(sorter, property);
    ensureIsSorted(sorter);
    docIds.sort((a, b)=>{
        const indexOfA = s.docs.get(getInternalDocumentId(sorter.sharedInternalDocumentStore, a[0]));
        const indexOfB = s.docs.get(getInternalDocumentId(sorter.sharedInternalDocumentStore, b[0]));
        const isAIndexed = typeof indexOfA !== 'undefined';
        const isBIndexed = typeof indexOfB !== 'undefined';
        if (!isAIndexed && !isBIndexed) {
            return 0;
        }
        if (!isAIndexed) {
            return 1;
        }
        if (!isBIndexed) {
            return -1;
        }
        return isDesc ? indexOfB - indexOfA : indexOfA - indexOfB;
    });
    return docIds;
}
async function getSortableProperties(sorter) {
    if (!sorter.enabled) {
        return [];
    }
    return sorter.sortableProperties;
}
async function getSortablePropertiesWithTypes(sorter) {
    if (!sorter.enabled) {
        return {};
    }
    return sorter.sortablePropertiesWithTypes;
}
async function load3(sharedInternalDocumentStore, raw) {
    const rawDocument = raw;
    if (!rawDocument.enabled) {
        return {
            enabled: false
        };
    }
    const sorts = Object.keys(rawDocument.sorts).reduce((acc, prop)=>{
        const { docs, orderedDocs, type } = rawDocument.sorts[prop];
        acc[prop] = {
            docs: new Map(Object.entries(docs).map(([k, v])=>[
                    +k,
                    v
                ])),
            orderedDocsToRemove: new Map(),
            orderedDocs,
            type
        };
        return acc;
    }, {});
    return {
        sharedInternalDocumentStore,
        language: rawDocument.language,
        sortableProperties: rawDocument.sortableProperties,
        sortablePropertiesWithTypes: rawDocument.sortablePropertiesWithTypes,
        sorts,
        enabled: true,
        isSorted: rawDocument.isSorted
    };
}
async function save3(sorter) {
    if (!sorter.enabled) {
        return {
            enabled: false
        };
    }
    ensureOrderedDocsAreDeleted(sorter);
    ensureIsSorted(sorter);
    const sorts = Object.keys(sorter.sorts).reduce((acc, prop)=>{
        const { docs, orderedDocs, type } = sorter.sorts[prop];
        acc[prop] = {
            docs: Object.fromEntries(docs.entries()),
            orderedDocs,
            type
        };
        return acc;
    }, {});
    return {
        language: sorter.language,
        sortableProperties: sorter.sortableProperties,
        sortablePropertiesWithTypes: sorter.sortablePropertiesWithTypes,
        sorts,
        enabled: sorter.enabled,
        isSorted: sorter.isSorted
    };
}
async function createSorter() {
    return {
        create: create6,
        insert: insert5,
        remove: remove3,
        save: save3,
        load: load3,
        sortBy,
        getSortableProperties,
        getSortablePropertiesWithTypes
    };
}
const CHARCODE_REPLACE_MAPPING = [
    65,
    65,
    65,
    65,
    65,
    65,
    65,
    67,
    69,
    69,
    69,
    69,
    73,
    73,
    73,
    73,
    69,
    78,
    79,
    79,
    79,
    79,
    79,
    null,
    79,
    85,
    85,
    85,
    85,
    89,
    80,
    115,
    97,
    97,
    97,
    97,
    97,
    97,
    97,
    99,
    101,
    101,
    101,
    101,
    105,
    105,
    105,
    105,
    101,
    110,
    111,
    111,
    111,
    111,
    111,
    null,
    111,
    117,
    117,
    117,
    117,
    121,
    112,
    121,
    65,
    97,
    65,
    97,
    65,
    97,
    67,
    99,
    67,
    99,
    67,
    99,
    67,
    99,
    68,
    100,
    68,
    100,
    69,
    101,
    69,
    101,
    69,
    101,
    69,
    101,
    69,
    101,
    71,
    103,
    71,
    103,
    71,
    103,
    71,
    103,
    72,
    104,
    72,
    104,
    73,
    105,
    73,
    105,
    73,
    105,
    73,
    105,
    73,
    105,
    73,
    105,
    74,
    106,
    75,
    107,
    107,
    76,
    108,
    76,
    108,
    76,
    108,
    76,
    108,
    76,
    108,
    78,
    110,
    78,
    110,
    78,
    110,
    110,
    78,
    110,
    79,
    111,
    79,
    111,
    79,
    111,
    79,
    111,
    82,
    114,
    82,
    114,
    82,
    114,
    83,
    115,
    83,
    115,
    83,
    115,
    83,
    115,
    84,
    116,
    84,
    116,
    84,
    116,
    85,
    117,
    85,
    117,
    85,
    117,
    85,
    117,
    85,
    117,
    85,
    117,
    87,
    119,
    89,
    121,
    89,
    90,
    122,
    90,
    122,
    90,
    122,
    115
];
function replaceChar(charCode) {
    if (charCode < 192 || charCode > 383) return charCode;
    return CHARCODE_REPLACE_MAPPING[charCode - 192] || charCode;
}
function replaceDiacritics(str) {
    const stringCharCode = [];
    for(let idx = 0; idx < str.length; idx++){
        stringCharCode[idx] = replaceChar(str.charCodeAt(idx));
    }
    return String.fromCharCode(...stringCharCode);
}
const step2List = {
    ational: 'ate',
    tional: 'tion',
    enci: 'ence',
    anci: 'ance',
    izer: 'ize',
    bli: 'ble',
    alli: 'al',
    entli: 'ent',
    eli: 'e',
    ousli: 'ous',
    ization: 'ize',
    ation: 'ate',
    ator: 'ate',
    alism: 'al',
    iveness: 'ive',
    fulness: 'ful',
    ousness: 'ous',
    aliti: 'al',
    iviti: 'ive',
    biliti: 'ble',
    logi: 'log'
};
const step3List = {
    icate: 'ic',
    ative: '',
    alize: 'al',
    iciti: 'ic',
    ical: 'ic',
    ful: '',
    ness: ''
};
const c = '[^aeiou]';
const v = '[aeiouy]';
const C = c + '[^aeiouy]*';
const V = v + '[aeiou]*';
const mgr0 = '^(' + C + ')?' + V + C;
const meq1 = '^(' + C + ')?' + V + C + '(' + V + ')?$';
const mgr1 = '^(' + C + ')?' + V + C + V + C;
const s_v = '^(' + C + ')?' + v;
function stemmer(w) {
    let stem;
    let suffix;
    let re;
    let re2;
    let re3;
    let re4;
    if (w.length < 3) {
        return w;
    }
    const firstch = w.substring(0, 1);
    if (firstch == 'y') {
        w = firstch.toUpperCase() + w.substring(1);
    }
    re = /^(.+?)(ss|i)es$/;
    re2 = /^(.+?)([^s])s$/;
    if (re.test(w)) {
        w = w.replace(re, '$1$2');
    } else if (re2.test(w)) {
        w = w.replace(re2, '$1$2');
    }
    re = /^(.+?)eed$/;
    re2 = /^(.+?)(ed|ing)$/;
    if (re.test(w)) {
        const fp = re.exec(w);
        re = new RegExp(mgr0);
        if (re.test(fp[1])) {
            re = /.$/;
            w = w.replace(re, '');
        }
    } else if (re2.test(w)) {
        const fp = re2.exec(w);
        stem = fp[1];
        re2 = new RegExp(s_v);
        if (re2.test(stem)) {
            w = stem;
            re2 = /(at|bl|iz)$/;
            re3 = new RegExp('([^aeiouylsz])\\1$');
            re4 = new RegExp('^' + C + v + '[^aeiouwxy]$');
            if (re2.test(w)) {
                w = w + 'e';
            } else if (re3.test(w)) {
                re = /.$/;
                w = w.replace(re, '');
            } else if (re4.test(w)) {
                w = w + 'e';
            }
        }
    }
    re = /^(.+?)y$/;
    if (re.test(w)) {
        const fp = re.exec(w);
        stem = fp === null || fp === void 0 ? void 0 : fp[1];
        re = new RegExp(s_v);
        if (stem && re.test(stem)) {
            w = stem + 'i';
        }
    }
    re = /^(.+?)(ational|tional|enci|anci|izer|bli|alli|entli|eli|ousli|ization|ation|ator|alism|iveness|fulness|ousness|aliti|iviti|biliti|logi)$/;
    if (re.test(w)) {
        const fp = re.exec(w);
        stem = fp === null || fp === void 0 ? void 0 : fp[1];
        suffix = fp === null || fp === void 0 ? void 0 : fp[2];
        re = new RegExp(mgr0);
        if (stem && re.test(stem)) {
            w = stem + step2List[suffix];
        }
    }
    re = /^(.+?)(icate|ative|alize|iciti|ical|ful|ness)$/;
    if (re.test(w)) {
        const fp = re.exec(w);
        stem = fp === null || fp === void 0 ? void 0 : fp[1];
        suffix = fp === null || fp === void 0 ? void 0 : fp[2];
        re = new RegExp(mgr0);
        if (stem && re.test(stem)) {
            w = stem + step3List[suffix];
        }
    }
    re = /^(.+?)(al|ance|ence|er|ic|able|ible|ant|ement|ment|ent|ou|ism|ate|iti|ous|ive|ize)$/;
    re2 = /^(.+?)(s|t)(ion)$/;
    if (re.test(w)) {
        const fp = re.exec(w);
        stem = fp === null || fp === void 0 ? void 0 : fp[1];
        re = new RegExp(mgr1);
        if (stem && re.test(stem)) {
            w = stem;
        }
    } else if (re2.test(w)) {
        const fp = re2.exec(w);
        stem = (fp === null || fp === void 0 ? void 0 : fp[1]) ?? '' + (fp === null || fp === void 0 ? void 0 : fp[2]) ?? '';
        re2 = new RegExp(mgr1);
        if (re2.test(stem)) {
            w = stem;
        }
    }
    re = /^(.+?)e$/;
    if (re.test(w)) {
        const fp = re.exec(w);
        stem = fp === null || fp === void 0 ? void 0 : fp[1];
        re = new RegExp(mgr1);
        re2 = new RegExp(meq1);
        re3 = new RegExp('^' + C + v + '[^aeiouwxy]$');
        if (stem && (re.test(stem) || re2.test(stem) && !re3.test(stem))) {
            w = stem;
        }
    }
    re = /ll$/;
    re2 = new RegExp(mgr1);
    if (re.test(w) && re2.test(w)) {
        re = /.$/;
        w = w.replace(re, '');
    }
    if (firstch == 'y') {
        w = firstch.toLowerCase() + w.substring(1);
    }
    return w;
}
function normalizeToken(prop, token) {
    var _this_stopWords;
    const key = `${this.language}:${prop}:${token}`;
    if (this.normalizationCache.has(key)) {
        return this.normalizationCache.get(key);
    }
    if ((_this_stopWords = this.stopWords) === null || _this_stopWords === void 0 ? void 0 : _this_stopWords.includes(token)) {
        this.normalizationCache.set(key, '');
        return '';
    }
    if (this.stemmer && !this.stemmerSkipProperties.has(prop)) {
        token = this.stemmer(token);
    }
    token = replaceDiacritics(token);
    this.normalizationCache.set(key, token);
    return token;
}
function trim(text) {
    while(text[text.length - 1] === ''){
        text.pop();
    }
    while(text[0] === ''){
        text.shift();
    }
    return text;
}
function tokenize(input, language, prop) {
    if (language && language !== this.language) {
        throw createError('LANGUAGE_NOT_SUPPORTED', language);
    }
    if (typeof input !== 'string') {
        return [
            input
        ];
    }
    let tokens;
    if (prop && this.tokenizeSkipProperties.has(prop)) {
        tokens = [
            this.normalizeToken.bind(this, prop ?? '')(input)
        ];
    } else {
        const splitRule = SPLITTERS[this.language];
        tokens = input.toLowerCase().split(splitRule).map(this.normalizeToken.bind(this, prop ?? '')).filter(Boolean);
    }
    const trimTokens = trim(tokens);
    if (!this.allowDuplicates) {
        return Array.from(new Set(trimTokens));
    }
    return trimTokens;
}
async function createTokenizer(config = {}) {
    if (!config.language) {
        config.language = 'english';
    } else if (!SUPPORTED_LANGUAGES.includes(config.language)) {
        throw createError('LANGUAGE_NOT_SUPPORTED', config.language);
    }
    let stemmer1;
    if (config.stemming || config.stemmer && !('stemming' in config)) {
        if (config.stemmer) {
            if (typeof config.stemmer !== 'function') {
                throw createError('INVALID_STEMMER_FUNCTION_TYPE');
            }
            stemmer1 = config.stemmer;
        } else {
            if (config.language === 'english') {
                stemmer1 = stemmer;
            } else {
                throw createError('MISSING_STEMMER', config.language);
            }
        }
    }
    let stopWords;
    if (config.stopWords !== false) {
        stopWords = [];
        if (Array.isArray(config.stopWords)) {
            stopWords = config.stopWords;
        } else if (typeof config.stopWords === 'function') {
            stopWords = await config.stopWords(stopWords);
        } else if (config.stopWords) {
            throw createError('CUSTOM_STOP_WORDS_MUST_BE_FUNCTION_OR_ARRAY');
        }
        if (!Array.isArray(stopWords)) {
            throw createError('CUSTOM_STOP_WORDS_MUST_BE_FUNCTION_OR_ARRAY');
        }
        for (const s of stopWords){
            if (typeof s !== 'string') {
                throw createError('CUSTOM_STOP_WORDS_MUST_BE_FUNCTION_OR_ARRAY');
            }
        }
    }
    const tokenizer = {
        tokenize,
        language: config.language,
        stemmer: stemmer1,
        stemmerSkipProperties: new Set(config.stemmerSkipProperties ? [
            config.stemmerSkipProperties
        ].flat() : []),
        tokenizeSkipProperties: new Set(config.tokenizeSkipProperties ? [
            config.tokenizeSkipProperties
        ].flat() : []),
        stopWords,
        allowDuplicates: Boolean(config.allowDuplicates),
        normalizeToken,
        normalizationCache: new Map()
    };
    tokenizer.tokenize = tokenize.bind(tokenizer);
    tokenizer.normalizeToken = normalizeToken;
    return tokenizer;
}
function validateComponents(components) {
    const defaultComponents = {
        formatElapsedTime,
        getDocumentIndexId,
        getDocumentProperties,
        validateSchema
    };
    for (const rawKey of FUNCTION_COMPONENTS){
        const key = rawKey;
        if (components[key]) {
            if (typeof components[key] !== 'function') {
                throw createError('COMPONENT_MUST_BE_FUNCTION', key);
            }
        } else {
            components[key] = defaultComponents[key];
        }
    }
    for (const rawKey of SINGLE_OR_ARRAY_COMPONENTS){
        const key = rawKey;
        const component = components[key];
        if (!component) {
            components[key] = [];
        } else if (!Array.isArray(components[key])) {
            components[key] = [
                components[key]
            ];
        }
        for (const fn of components[key]){
            if (typeof fn !== 'function') {
                throw createError('COMPONENT_MUST_BE_FUNCTION_OR_ARRAY_FUNCTIONS', key);
            }
        }
    }
    for (const rawKey of Object.keys(components)){
        if (!OBJECT_COMPONENTS.includes(rawKey) && !FUNCTION_COMPONENTS.includes(rawKey) && !SINGLE_OR_ARRAY_COMPONENTS.includes(rawKey)) {
            throw createError('UNSUPPORTED_COMPONENT', rawKey);
        }
    }
}
async function create7({ schema, sort, language, components, id }) {
    if (!components) {
        components = {};
    }
    if (!id) {
        id = await uniqueId();
    }
    let tokenizer = components.tokenizer;
    let index = components.index;
    let documentsStore = components.documentsStore;
    let sorter = components.sorter;
    if (!tokenizer) {
        tokenizer = await createTokenizer({
            language: language ?? 'english'
        });
    } else if (!tokenizer.tokenize) {
        tokenizer = await createTokenizer(tokenizer);
    } else {
        const customTokenizer = tokenizer;
        tokenizer = customTokenizer;
    }
    if (components.tokenizer && language) {
        throw createError('NO_LANGUAGE_WITH_CUSTOM_TOKENIZER');
    }
    const internalDocumentStore = createInternalDocumentIDStore();
    index ||= await createIndex();
    sorter ||= await createSorter();
    documentsStore ||= await createDocumentsStore();
    validateComponents(components);
    const { getDocumentProperties, getDocumentIndexId, validateSchema, beforeInsert, afterInsert, beforeRemove, afterRemove, beforeUpdate, afterUpdate, afterSearch, beforeMultipleInsert, afterMultipleInsert, beforeMultipleRemove, afterMultipleRemove, beforeMultipleUpdate, afterMultipleUpdate, formatElapsedTime } = components;
    const orama = {
        data: {},
        caches: {},
        schema,
        tokenizer,
        index,
        sorter,
        documentsStore,
        internalDocumentIDStore: internalDocumentStore,
        getDocumentProperties,
        getDocumentIndexId,
        validateSchema,
        beforeInsert,
        afterInsert,
        beforeRemove,
        afterRemove,
        beforeUpdate,
        afterUpdate,
        afterSearch,
        beforeMultipleInsert,
        afterMultipleInsert,
        beforeMultipleRemove,
        afterMultipleRemove,
        beforeMultipleUpdate,
        afterMultipleUpdate,
        formatElapsedTime,
        id
    };
    orama.data = {
        index: await orama.index.create(orama, internalDocumentStore, schema),
        docs: await orama.documentsStore.create(orama, internalDocumentStore),
        sorting: await orama.sorter.create(orama, internalDocumentStore, schema, sort)
    };
    return orama;
}

const kInsertions = Symbol('orama.insertions');
const kRemovals = Symbol('orama.removals');
export { kInsertions as kInsertions };
export { kRemovals as kRemovals };
var _globalThis_process;
const warn = ((_globalThis_process = globalThis.process) === null || _globalThis_process === void 0 ? void 0 : _globalThis_process.emitWarning) ?? function emitWarning(message, options) {
    console.warn(`[WARNING] [${options.code}] ${message}`);
};
function trackInsertion(orama) {
    if (typeof orama[kInsertions] !== 'number') {
        queueMicrotask(()=>{
            orama[kInsertions] = undefined;
        });
        orama[kInsertions] = 0;
    }
    if (orama[kInsertions] > 1000) {
        warn("Orama's insert operation is synchronous. Please avoid inserting a large number of document in a single operation in order not to block the main thread or, in alternative, please use insertMultiple.", {
            code: 'ORAMA0001'
        });
        orama[kInsertions] = -1;
    } else if (orama[kInsertions] >= 0) {
        orama[kInsertions]++;
    }
}
async function insert6(orama, doc, language, skipHooks) {
    const errorProperty = await orama.validateSchema(doc, orama.schema);
    if (errorProperty) {
        throw createError('SCHEMA_VALIDATION_FAILURE', errorProperty);
    }
    return innerInsert(orama, doc, language, skipHooks);
}
async function innerInsert(orama, doc, language, skipHooks) {
    const { index, docs } = orama.data;
    const id = await orama.getDocumentIndexId(doc);
    if (typeof id !== 'string') {
        throw createError('DOCUMENT_ID_MUST_BE_STRING', typeof id);
    }
    if (!await orama.documentsStore.store(docs, id, doc)) {
        throw createError('DOCUMENT_ALREADY_EXISTS', id);
    }
    const docsCount = await orama.documentsStore.count(docs);
    if (!skipHooks) {
        await runSingleHook(orama.beforeInsert, orama, id, doc);
    }
    const indexableProperties = await orama.index.getSearchableProperties(index);
    const indexablePropertiesWithTypes = await orama.index.getSearchablePropertiesWithTypes(index);
    const indexableValues = await orama.getDocumentProperties(doc, indexableProperties);
    for (const [key, value] of Object.entries(indexableValues)){
        if (typeof value === 'undefined') {
            continue;
        }
        const actualType = typeof value;
        const expectedType = indexablePropertiesWithTypes[key];
        if (isGeoPointType(expectedType) && typeof value === 'object' && typeof value.lon === 'number' && typeof value.lat === 'number') {
            continue;
        }
        if (isVectorType(expectedType) && Array.isArray(value)) {
            continue;
        }
        if (isArrayType(expectedType) && Array.isArray(value)) {
            continue;
        }
        if ((expectedType === 'enum' || expectedType === 'enum[]') && (actualType === 'string' || actualType === 'number')) {
            continue;
        }
        if (actualType !== expectedType) {
            throw createError('INVALID_DOCUMENT_PROPERTY', key, expectedType, actualType);
        }
    }
    for (const prop of indexableProperties){
        var _orama_index, _orama_index_beforeInsert, _orama_index1, _orama_index_afterInsert;
        const value = indexableValues[prop];
        if (typeof value === 'undefined') {
            continue;
        }
        const expectedType = indexablePropertiesWithTypes[prop];
        await ((_orama_index_beforeInsert = (_orama_index = orama.index).beforeInsert) === null || _orama_index_beforeInsert === void 0 ? void 0 : _orama_index_beforeInsert.call(_orama_index, orama.data.index, prop, id, value, expectedType, language, orama.tokenizer, docsCount));
        await orama.index.insert(orama.index, orama.data.index, prop, id, value, expectedType, language, orama.tokenizer, docsCount);
        await ((_orama_index_afterInsert = (_orama_index1 = orama.index).afterInsert) === null || _orama_index_afterInsert === void 0 ? void 0 : _orama_index_afterInsert.call(_orama_index1, orama.data.index, prop, id, value, expectedType, language, orama.tokenizer, docsCount));
    }
    const sortableProperties = await orama.sorter.getSortableProperties(orama.data.sorting);
    const sortablePropertiesWithTypes = await orama.sorter.getSortablePropertiesWithTypes(orama.data.sorting);
    const sortableValues = await orama.getDocumentProperties(doc, sortableProperties);
    for (const prop of sortableProperties){
        const value = sortableValues[prop];
        if (typeof value === 'undefined') {
            continue;
        }
        const expectedType = sortablePropertiesWithTypes[prop];
        await orama.sorter.insert(orama.data.sorting, prop, id, value, expectedType, language);
    }
    if (!skipHooks) {
        await runSingleHook(orama.afterInsert, orama, id, doc);
    }
    trackInsertion(orama);
    return id;
}
async function insertMultiple(orama, docs, batchSize, language, skipHooks) {
    if (!skipHooks) {
        await runMultipleHook(orama.beforeMultipleInsert, orama, docs);
    }
    const docsLength = docs.length;
    for(let i = 0; i < docsLength; i++){
        const errorProperty = await orama.validateSchema(docs[i], orama.schema);
        if (errorProperty) {
            throw createError('SCHEMA_VALIDATION_FAILURE', errorProperty);
        }
    }
    return innerInsertMultiple(orama, docs, batchSize, language, skipHooks);
}
async function innerInsertMultiple(orama, docs, batchSize, language, skipHooks) {
    if (!batchSize) {
        batchSize = 1000;
    }
    const ids = [];
    await new Promise((resolve, reject)=>{
        let i = 0;
        async function _insertMultiple() {
            const batch = docs.slice(i * batchSize, (i + 1) * batchSize);
            i++;
            if (!batch.length) {
                return resolve();
            }
            for (const doc of batch){
                try {
                    const id = await insert6(orama, doc, language, skipHooks);
                    ids.push(id);
                } catch (err) {
                    reject(err);
                }
            }
            setTimeout(_insertMultiple, 0);
        }
        setTimeout(_insertMultiple, 0);
    });
    if (!skipHooks) {
        await runMultipleHook(orama.afterMultipleInsert, orama, docs);
    }
    return ids;
}
function sortingPredicate(order = 'desc', a, b) {
    if (order.toLowerCase() === 'asc') {
        return a[1] - b[1];
    } else {
        return b[1] - a[1];
    }
}
async function getFacets(orama, results, facetsConfig) {
    const facets = {};
    const allIDs = results.map(([id])=>id);
    const allDocs = await orama.documentsStore.getMultiple(orama.data.docs, allIDs);
    const facetKeys = Object.keys(facetsConfig);
    const properties = await orama.index.getSearchablePropertiesWithTypes(orama.data.index);
    for (const facet of facetKeys){
        let values = {};
        if (properties[facet] === 'number') {
            const { ranges } = facetsConfig[facet];
            const tmp = [];
            for (const range of ranges){
                tmp.push([
                    `${range.from}-${range.to}`,
                    0
                ]);
            }
            values = Object.fromEntries(tmp);
        }
        facets[facet] = {
            count: 0,
            values
        };
    }
    const allDocsLength = allDocs.length;
    for(let i = 0; i < allDocsLength; i++){
        const doc = allDocs[i];
        for (const facet of facetKeys){
            const facetValue = facet.includes('.') ? await getNested(doc, facet) : doc[facet];
            const propertyType = properties[facet];
            switch(propertyType){
                case 'number':
                    {
                        const ranges = facetsConfig[facet].ranges;
                        calculateNumberFacet(ranges, facets[facet].values, facetValue);
                        break;
                    }
                case 'number[]':
                    {
                        const alreadyInsertedValues = new Set();
                        const ranges = facetsConfig[facet].ranges;
                        for (const v of facetValue){
                            calculateNumberFacet(ranges, facets[facet].values, v, alreadyInsertedValues);
                        }
                        break;
                    }
                case 'boolean':
                case 'enum':
                case 'string':
                    {
                        calculateBooleanStringOrEnumFacet(facets[facet].values, facetValue, propertyType);
                        break;
                    }
                case 'boolean[]':
                case 'enum[]':
                case 'string[]':
                    {
                        const alreadyInsertedValues = new Set();
                        const innerType = propertyType === 'boolean[]' ? 'boolean' : 'string';
                        for (const v of facetValue){
                            calculateBooleanStringOrEnumFacet(facets[facet].values, v, innerType, alreadyInsertedValues);
                        }
                        break;
                    }
                default:
                    throw createError('FACET_NOT_SUPPORTED', propertyType);
            }
        }
    }
    for (const facet of facetKeys){
        facets[facet].count = Object.keys(facets[facet].values).length;
        if (properties[facet] === 'string') {
            const stringFacetDefinition = facetsConfig;
            facets[facet].values = Object.fromEntries(Object.entries(facets[facet].values).sort((a, b)=>sortingPredicate(stringFacetDefinition.sort, a, b)).slice(stringFacetDefinition.offset ?? 0, stringFacetDefinition.limit ?? 10));
        }
    }
    return facets;
}
function calculateNumberFacet(ranges, values, facetValue, alreadyInsertedValues) {
    for (const range of ranges){
        const value = `${range.from}-${range.to}`;
        if (alreadyInsertedValues && alreadyInsertedValues.has(value)) {
            continue;
        }
        if (facetValue >= range.from && facetValue <= range.to) {
            if (values[value] === undefined) {
                values[value] = 1;
            } else {
                values[value]++;
                if (alreadyInsertedValues) {
                    alreadyInsertedValues.add(value);
                }
            }
        }
    }
}
function calculateBooleanStringOrEnumFacet(values, facetValue, propertyType, alreadyInsertedValues) {
    const value = (facetValue === null || facetValue === void 0 ? void 0 : facetValue.toString()) ?? (propertyType === 'boolean' ? 'false' : '');
    if (alreadyInsertedValues && alreadyInsertedValues.has(value)) {
        return;
    }
    values[value] = (values[value] ?? 0) + 1;
    if (alreadyInsertedValues) {
        alreadyInsertedValues.add(value);
    }
}
function intersectFilteredIDs(filtered, lookedUp) {
    const map = new Map();
    const result = [];
    for (const id of filtered){
        map.set(id, true);
    }
    for (const [id, score] of lookedUp){
        if (map.has(id)) {
            result.push([
                id,
                score
            ]);
            map.delete(id);
        }
    }
    return result;
}
const DEFAULT_REDUCE = {
    reducer: (_, acc, res, index)=>{
        acc[index] = res;
        return acc;
    },
    getInitialValue: (length)=>Array.from({
            length
        })
};
const ALLOWED_TYPES = [
    'string',
    'number',
    'boolean'
];
async function getGroups(orama, results, groupBy) {
    const properties = groupBy.properties;
    const propertiesLength = properties.length;
    const schemaProperties = await orama.index.getSearchablePropertiesWithTypes(orama.data.index);
    for(let i = 0; i < propertiesLength; i++){
        const property = properties[i];
        if (typeof schemaProperties[property] === 'undefined') {
            throw createError('UNKNOWN_GROUP_BY_PROPERTY', property);
        }
        if (!ALLOWED_TYPES.includes(schemaProperties[property])) {
            throw createError('INVALID_GROUP_BY_PROPERTY', property, ALLOWED_TYPES.join(', '), schemaProperties[property]);
        }
    }
    const allIDs = results.map(([id])=>getDocumentIdFromInternalId(orama.internalDocumentIDStore, id));
    const allDocs = await orama.documentsStore.getMultiple(orama.data.docs, allIDs);
    const allDocsLength = allDocs.length;
    const returnedCount = groupBy.maxResult || Number.MAX_SAFE_INTEGER;
    const listOfValues = [];
    const g = {};
    for(let i = 0; i < propertiesLength; i++){
        const groupByKey = properties[i];
        const group = {
            property: groupByKey,
            perValue: {}
        };
        const values = new Set();
        for(let j = 0; j < allDocsLength; j++){
            const doc = allDocs[j];
            const value = await getNested(doc, groupByKey);
            if (typeof value === 'undefined') {
                continue;
            }
            const keyValue = typeof value !== 'boolean' ? value : '' + value;
            if (typeof group.perValue[keyValue] === 'undefined') {
                group.perValue[keyValue] = {
                    indexes: [],
                    count: 0
                };
            }
            if (group.perValue[keyValue].count >= returnedCount) {
                continue;
            }
            group.perValue[keyValue].indexes.push(j);
            group.perValue[keyValue].count++;
            values.add(value);
        }
        listOfValues.push(Array.from(values));
        g[groupByKey] = group;
    }
    const combinations = calculateCombination(listOfValues);
    const combinationsLength = combinations.length;
    const groups = [];
    for(let i = 0; i < combinationsLength; i++){
        const combination = combinations[i];
        const combinationLength = combination.length;
        const group = {
            values: [],
            indexes: []
        };
        const indexes = [];
        for(let j = 0; j < combinationLength; j++){
            const value = combination[j];
            const property = properties[j];
            indexes.push(g[property].perValue[typeof value !== 'boolean' ? value : '' + value].indexes);
            group.values.push(value);
        }
        group.indexes = intersect(indexes).sort((a, b)=>a - b);
        if (group.indexes.length === 0) {
            continue;
        }
        groups.push(group);
    }
    const groupsLength = groups.length;
    const res = Array.from({
        length: groupsLength
    });
    for(let i = 0; i < groupsLength; i++){
        const group = groups[i];
        const reduce = groupBy.reduce || DEFAULT_REDUCE;
        const docs = group.indexes.map((index)=>{
            return {
                id: allIDs[index],
                score: results[index][1],
                document: allDocs[index]
            };
        });
        const func = reduce.reducer.bind(null, group.values);
        const initialValue = reduce.getInitialValue(group.indexes.length);
        const aggregationValue = docs.reduce(func, initialValue);
        res[i] = {
            values: group.values,
            result: aggregationValue
        };
    }
    return res;
}
function calculateCombination(arrs, index = 0) {
    if (index + 1 === arrs.length) return arrs[index].map((item)=>[
            item
        ]);
    const head = arrs[index];
    const c = calculateCombination(arrs, index + 1);
    const combinations = [];
    for (const value of head){
        for (const combination of c){
            const result = [
                value
            ];
            safeArrayPush(result, combination);
            combinations.push(result);
        }
    }
    return combinations;
}
const defaultBM25Params = {
    k: 1.2,
    b: 0.75,
    d: 0.5
};
async function createSearchContext(tokenizer, index, documentsStore, language, params, properties, tokens, docsCount, timeStart) {
    const indexMap = {};
    const docsIntersection = {};
    for (const prop of properties){
        const tokensMap = {};
        for (const token of tokens){
            tokensMap[token] = [];
        }
        indexMap[prop] = tokensMap;
        docsIntersection[prop] = [];
    }
    return {
        timeStart,
        tokenizer,
        index,
        documentsStore,
        language,
        params,
        docsCount,
        uniqueDocsIDs: {},
        indexMap,
        docsIntersection
    };
}
async function search1(orama, params, language) {
    const timeStart = await getNanosecondsTime();
    params.relevance = Object.assign(params.relevance ?? {}, defaultBM25Params);
    const shouldCalculateFacets = params.facets && Object.keys(params.facets).length > 0;
    const { limit = 10, offset = 0, term, properties, threshold = 1, distinctOn } = params;
    const isPreflight = params.preflight === true;
    const { index, docs } = orama.data;
    const tokens = await orama.tokenizer.tokenize(term ?? '', language);
    let propertiesToSearch = orama.caches['propertiesToSearch'];
    if (!propertiesToSearch) {
        const propertiesToSearchWithTypes = await orama.index.getSearchablePropertiesWithTypes(index);
        propertiesToSearch = await orama.index.getSearchableProperties(index);
        propertiesToSearch = propertiesToSearch.filter((prop)=>propertiesToSearchWithTypes[prop].startsWith('string'));
        orama.caches['propertiesToSearch'] = propertiesToSearch;
    }
    if (properties && properties !== '*') {
        for (const prop of properties){
            if (!propertiesToSearch.includes(prop)) {
                throw createError('UNKNOWN_INDEX', prop, propertiesToSearch.join(', '));
            }
        }
        propertiesToSearch = propertiesToSearch.filter((prop)=>properties.includes(prop));
    }
    const context = await createSearchContext(orama.tokenizer, orama.index, orama.documentsStore, language, params, propertiesToSearch, tokens, await orama.documentsStore.count(docs), timeStart);
    const hasFilters = Object.keys(params.where ?? {}).length > 0;
    let whereFiltersIDs = [];
    if (hasFilters) {
        whereFiltersIDs = await orama.index.searchByWhereClause(context, index, params.where);
    }
    const tokensLength = tokens.length;
    if (tokensLength || properties && properties.length > 0) {
        const indexesLength = propertiesToSearch.length;
        for(let i = 0; i < indexesLength; i++){
            var _params_boost;
            const prop = propertiesToSearch[i];
            if (tokensLength !== 0) {
                for(let j = 0; j < tokensLength; j++){
                    const term = tokens[j];
                    const scoreList = await orama.index.search(context, index, prop, term);
                    safeArrayPush(context.indexMap[prop][term], scoreList);
                }
            } else {
                context.indexMap[prop][''] = [];
                const scoreList = await orama.index.search(context, index, prop, '');
                safeArrayPush(context.indexMap[prop][''], scoreList);
            }
            const docIds = context.indexMap[prop];
            const vals = Object.values(docIds);
            context.docsIntersection[prop] = prioritizeTokenScores(vals, (params === null || params === void 0 ? void 0 : (_params_boost = params.boost) === null || _params_boost === void 0 ? void 0 : _params_boost[prop]) ?? 1, threshold, tokensLength);
            const uniqueDocs = context.docsIntersection[prop];
            const uniqueDocsLength = uniqueDocs.length;
            for(let i = 0; i < uniqueDocsLength; i++){
                const [id, score] = uniqueDocs[i];
                const prevScore = context.uniqueDocsIDs[id];
                if (prevScore) {
                    context.uniqueDocsIDs[id] = prevScore + score + 0.5;
                } else {
                    context.uniqueDocsIDs[id] = score;
                }
            }
        }
    } else if (tokens.length === 0 && term) {
        context.uniqueDocsIDs = {};
    } else {
        context.uniqueDocsIDs = Object.fromEntries(Object.keys(await orama.documentsStore.getAll(orama.data.docs)).map((k)=>[
                k,
                0
            ]));
    }
    let uniqueDocsArray = Object.entries(context.uniqueDocsIDs).map(([id, score])=>[
            +id,
            score
        ]);
    if (hasFilters) {
        uniqueDocsArray = intersectFilteredIDs(whereFiltersIDs, uniqueDocsArray);
    }
    if (params.sortBy) {
        if (typeof params.sortBy === 'function') {
            const ids = uniqueDocsArray.map(([id])=>id);
            const docs = await orama.documentsStore.getMultiple(orama.data.docs, ids);
            const docsWithIdAndScore = docs.map((d, i)=>[
                    uniqueDocsArray[i][0],
                    uniqueDocsArray[i][1],
                    d
                ]);
            docsWithIdAndScore.sort(params.sortBy);
            uniqueDocsArray = docsWithIdAndScore.map(([id, score])=>[
                    id,
                    score
                ]);
        } else {
            uniqueDocsArray = await orama.sorter.sortBy(orama.data.sorting, uniqueDocsArray, params.sortBy).then((results)=>results.map(([id, score])=>[
                        getInternalDocumentId(orama.internalDocumentIDStore, id),
                        score
                    ]));
        }
    } else {
        uniqueDocsArray = uniqueDocsArray.sort(sortTokenScorePredicate);
    }
    let results;
    if (!isPreflight && distinctOn) {
        results = await fetchDocumentsWithDistinct(orama, uniqueDocsArray, offset, limit, distinctOn);
    } else if (!isPreflight) {
        results = await fetchDocuments(orama, uniqueDocsArray, offset, limit);
    }
    const searchResult = {
        elapsed: {
            formatted: '',
            raw: 0
        },
        hits: [],
        count: uniqueDocsArray.length
    };
    if (typeof results !== 'undefined') {
        searchResult.hits = results.filter(Boolean);
    }
    if (shouldCalculateFacets) {
        const facets = await getFacets(orama, uniqueDocsArray, params.facets);
        searchResult.facets = facets;
    }
    if (params.groupBy) {
        searchResult.groups = await getGroups(orama, uniqueDocsArray, params.groupBy);
    }
    if (orama.afterSearch) {
        await runAfterSearch(orama.afterSearch, orama, params, language, searchResult);
    }
    searchResult.elapsed = await orama.formatElapsedTime(await getNanosecondsTime() - context.timeStart);
    return searchResult;
}
async function fetchDocumentsWithDistinct(orama, uniqueDocsArray, offset, limit, distinctOn) {
    const docs = orama.data.docs;
    const values = new Map();
    const results = [];
    const resultIDs = new Set();
    const uniqueDocsArrayLength = uniqueDocsArray.length;
    let count = 0;
    for(let i = 0; i < uniqueDocsArrayLength; i++){
        const idAndScore = uniqueDocsArray[i];
        if (typeof idAndScore === 'undefined') {
            continue;
        }
        const [id, score] = idAndScore;
        if (resultIDs.has(id)) {
            continue;
        }
        const doc = await orama.documentsStore.get(docs, id);
        const value = await getNested(doc, distinctOn);
        if (typeof value === 'undefined' || values.has(value)) {
            continue;
        }
        values.set(value, true);
        count++;
        if (count <= offset) {
            continue;
        }
        results.push({
            id: getDocumentIdFromInternalId(orama.internalDocumentIDStore, id),
            score,
            document: doc
        });
        resultIDs.add(id);
        if (count >= offset + limit) {
            break;
        }
    }
    return results;
}
async function fetchDocuments(orama, uniqueDocsArray, offset, limit) {
    const docs = orama.data.docs;
    const results = Array.from({
        length: limit
    });
    const resultIDs = new Set();
    for(let i = offset; i < limit + offset; i++){
        const idAndScore = uniqueDocsArray[i];
        if (typeof idAndScore === 'undefined') {
            break;
        }
        const [id, score] = idAndScore;
        if (!resultIDs.has(id)) {
            const fullDoc = await orama.documentsStore.get(docs, id);
            results[i] = {
                id: getDocumentIdFromInternalId(orama.internalDocumentIDStore, id),
                score,
                document: fullDoc
            };
            resultIDs.add(id);
        }
    }
    return results;
}
export { create7 as create };
export { insertMultiple as insertMultiple };
export { search1 as search };
