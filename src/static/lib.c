#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef struct String {
    void *data;
} string;

typedef struct {
    enum { UNIT, BOOL, INT, FLOAT, STRING } type;
    union {
        int Unit;
        int Bool;
        int64_t Int;
        double Float;
        string String;
    } data;
} Value;

typedef struct {
    enum { OK, ERR } type;
    union {
        Value Ok;
        Value Err;
    } data;
} Result;

#define Unit() ((Value){UNIT, .data.Unit = 0})
#define Bool(x) ((Value){BOOL, .data.Bool = x})
#define Int(x) ((Value){INT, .data.Int = x})
#define Float(x) ((Value){FLOAT, .data.Float = x})
#define String(x) ((Value){STRING, .data.String = to_string(x)})

#define EXPECT_BOOL(x)                                                         \
    ({                                                                         \
        if (x.type != BOOL)                                                    \
            return Err(String("Expected boolean"));                            \
        x.data.Bool;                                                           \
    })

#define Ok(x) ((Result){OK, .data.Ok = x})
#define Err(x) ((Result){ERR, .data.Err = x})

#define Try(x)                                                                 \
    ({                                                                         \
        Result res = x;                                                        \
        if (res.type == ERR)                                                   \
            return res;                                                        \
        res.data.Ok;                                                           \
    })

string to_string(char *);

Result add(Value x, Value y);
Result sub(Value x, Value y);
Result less(Value x, Value y);

char *nameof(Value x) {
    switch (x.type) {
    case UNIT:
        return "unit";
    case BOOL:
        return "bool";
    case INT:
        return "int";
    case FLOAT:
        return "float";
    case STRING:
        return "string";
    }
}

Result print(Value x);
Result println(Value x);

string to_string(char *x) { return (string){.data = x}; }

Result add(Value x, Value y) {
    Value res;

    switch (x.type) {
    case INT:
        switch (y.type) {
        case INT:
            res = Int(x.data.Int + y.data.Int);
            break;
        case FLOAT:
            res = Float(x.data.Int + y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in add"));
        }
        break;
    case FLOAT:
        switch (y.type) {
        case INT:
            res = Float(x.data.Float + y.data.Int);
            break;
        case FLOAT:
            res = Float(x.data.Float + y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in add"));
        }
        break;
    case STRING:
        switch (y.type) {
        case STRING: {
            int total_length =
                strlen(x.data.String.data) + strlen(y.data.String.data);
            char *result = malloc(total_length + 1);
            strcpy(result, x.data.String.data);
            strcat(result, y.data.String.data);
            res = String(result);
            break;
        }
        default:
            return Err(String("Type mismatch in add"));
        }
        break;
    default:
        return Err(String("Type mismatch in add"));
    }

    return Ok(res);
}

Result sub(Value x, Value y) {
    Value res;

    switch (x.type) {
    case INT:
        switch (y.type) {
        case INT:
            res = Int(x.data.Int - y.data.Int);
            break;
        case FLOAT:
            res = Float(x.data.Int - y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in add"));
        }
        break;
    case FLOAT:
        switch (y.type) {
        case INT:
            res = Float(x.data.Float - y.data.Int);
            break;
        case FLOAT:
            res = Float(x.data.Float - y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in add"));
        }
        break;
    default:
        return Err(String("Type mismatch in add"));
    }

    return Ok(res);
}

Result less(Value x, Value y) {
    Value res;

    switch (x.type) {
    case INT:
        switch (y.type) {
        case INT:
            res = Bool(x.data.Int < y.data.Int);
            break;
        case FLOAT:
            res = Bool(x.data.Int < y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in less"));
        }
        break;
    case FLOAT:
        switch (y.type) {
        case INT:
            res = Bool(x.data.Float < y.data.Int);
            break;
        case FLOAT:
            res = Bool(x.data.Float < y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in less"));
        }
        break;
    default:
        return Err(String("Type mismatch in less"));
    }

    return Ok(res);
}

Result mul(Value x, Value y) {
    Value res;

    switch (x.type) {
    case INT:
        switch (y.type) {
        case INT:
            res = Int(x.data.Int * y.data.Int);
            break;
        case FLOAT:
            res = Float(x.data.Int * y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in mul"));
        }
        break;
    case FLOAT:
        switch (y.type) {
        case INT:
            res = Float(x.data.Float * y.data.Int);
            break;
        case FLOAT:
            res = Float(x.data.Float * y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in mul"));
        }
        break;
    default:
        return Err(String("Type mismatch in mul"));
    }

    return Ok(res);
}

Result equals(Value x, Value y) {
    if (x.type != y.type)
        return Ok(Bool(0));

    Value res;

    switch (x.type) {
    case INT:
        switch (y.type) {
        case INT:
            res = Bool(x.data.Int == y.data.Int);
            break;
        case FLOAT:
            res = Bool(x.data.Int == y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in equals"));
        }
        break;
    case FLOAT:
        switch (y.type) {
        case INT:
            res = Bool(x.data.Float == y.data.Int);
            break;
        case FLOAT:
            res = Bool(x.data.Float == y.data.Float);
            break;
        default:
            return Err(String("Type mismatch in equals"));
        }
        break;
    case STRING:
        switch (y.type) {
        case STRING:
            res = Bool(strcmp(x.data.String.data, y.data.String.data) == 0);
            break;
        default:
            return Err(String("Type mismatch in equals"));
        }
        break;
    default:
        return Err(String("Type mismatch in equals"));
    }

    return Ok(res);
}

Result print(Value x) {
    switch (x.type) {
    case UNIT:
        printf("()");
        break;
    case BOOL:
        printf("%s", x.data.Bool ? "true" : "false");
        break;
    case INT:
        printf("%ld", x.data.Int);
        break;
    case FLOAT:
        printf("%f", x.data.Float);
        break;
    case STRING:
        printf("%s", (char *)x.data.String.data);
        break;
    }

    return Ok(Unit());
}

Result println(Value x) {
    print(x);
    printf("\n");

    return Ok(Unit());
}