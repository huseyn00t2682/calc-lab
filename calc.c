// HUSEYN TAHIRZADE 251ADB188
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// resources used are: cppreference.com for strtod/strtoll for syntaxs,w3schools for C string handlings and for some standard c programming notes

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>

enum { T_NUM, T_PLUS, T_MINUS, T_MUL, T_DIV, T_POW, T_LPAREN, T_RPAREN, T_EOL, T_EOF, T_ERR };

typedef struct {
int is_f;
long long i;
double f;
} val_t;

typedef struct {
char *txt;
char *base;
int tok;
char *tok_ptr;
int err_pos;
val_t val;
} ctx_t;

// it calculates 1-based char's position from base bufffer
void set_err(ctx_t *c, char *p) {
    if (c->err_pos == 0) {
    c->err_pos = (p - c->base) + 1;
    }
}

// now the tokenizer tracks tokens' locations and exact character's offsets
void next_tok(ctx_t *c) {
    char *p = c->txt;
    while (*p == ' ' || *p == '\t') p++;
    
    c->tok_ptr = p;
    
    if (*p == '\0') {
    c->tok = T_EOF;
    c->txt = p;
    return;}

    if (*p == '\n') {
    c->tok = T_EOL;
    c->txt = p;
    return;}

    if (isdigit(*p) || *p == '.') {
        char *start = p;
        int has_dot = 0;
        char *first_dot = NULL;
        while (isdigit(*p) || *p == '.') {
            if (*p == '.') {
                has_dot++;
                if (!first_dot) first_dot = p;}p++;}
        
        if (start[0] == '.') {// leading to thedot (.5)
            set_err(c, start);
            c->tok = T_ERR;
            return;}
        if (p[-1] == '.') {// trailing to the dot (5.)
            set_err(c, p - 1);
            c->tok = T_ERR;
            return;}
        
        if (has_dot > 1) {// multiple dots
            set_err(c, first_dot);
            c->tok = T_ERR;
            return;  }
        c->val.is_f = has_dot;
        if (has_dot) c->val.f = strtod(start, NULL);
        else c->val.i = strtoll(start, NULL, 10);
        c->tok = T_NUM;
        c->txt = p;
        return;
    }
    
    if (*p == '+') { c->tok = T_PLUS; c->txt = p + 1; return; }
    if (*p == '-') { c->tok = T_MINUS; c->txt = p + 1; return; }
    if (*p == '*') {
        if (p[1] == '*') { c->tok = T_POW; c->txt = p + 2; return; }
        c->tok = T_MUL; c->txt = p + 1; return; }
    if (*p == '/') { c->tok = T_DIV; c->txt = p + 1; return; }
    if (*p == '(') { c->tok = T_LPAREN; c->txt = p + 1; return; }
    if (*p == ')') { c->tok = T_RPAREN; c->txt = p + 1; return; }
    
    set_err(c, p);
    c->tok = T_ERR;}

val_t parse_expr(ctx_t *c);

val_t parse_primary(ctx_t *c) {
    val_t res = {0, 0, 0.0};
    if (c->tok == T_NUM) {
        res = c->val;
        next_tok(c);
    } else if (c->tok == T_LPAREN) {
        next_tok(c);
        res = parse_expr(c);
        if (c->tok != T_RPAREN) {
            if (c->err_pos == 0) set_err(c, c->tok_ptr);
        } else {
            next_tok(c);
        }
    } else {
        if (c->err_pos == 0) set_err(c, c->tok_ptr);
    }
    return res;
}

val_t parse_power(ctx_t *c) {
    val_t left = parse_primary(c);
    if (c->tok == T_POW) {
        char *pow_op_ptr = c->tok_ptr;
        next_tok(c);
        char *right_ptr = c->tok_ptr;
        val_t right = parse_power(c);
        if (c->err_pos) return left;
        
        if (left.is_f || right.is_f) {
            double base = left.is_f ? left.f : (double)left.i;
            double exp = right.is_f ? right.f : (double)right.i;
            left.is_f = 1;
            left.f = pow(base, exp);
            if (isinf(left.f) || isnan(left.f)) set_err(c, pow_op_ptr);
        } else {
            if (right.i < 0) {
                set_err(c, right_ptr);
            } else {
                long long base = left.i;
                long long exp = right.i;
                long long res = 1;
                while (exp > 0) {
                if (exp % 2 == 1) res *= base;
                base *= base;
                exp /= 2;
                }
                left.i = res; }}}
    return left;
}

val_t parse_term(ctx_t *c) {
    val_t left = parse_power(c);
    while (c->tok == T_MUL || c->tok == T_DIV) {
        int op = c->tok;
        next_tok(c);
        char *right_ptr = c->tok_ptr;
        val_t right = parse_power(c);
        if (c->err_pos) return left;
        
        if (left.is_f || right.is_f) {
            double l = left.is_f ? left.f : (double)left.i;
            double r = right.is_f ? right.f : (double)right.i;
            left.is_f = 1;
            if (op == T_MUL) left.f = l * r;
            else {
                if (r == 0.0) { set_err(c, right_ptr); }
                else left.f = l / r;
            }
        } else {
            if (op == T_MUL) left.i *= right.i;
            else {
                if (right.i == 0) { set_err(c, right_ptr); }
                else left.i /= right.i;}}}
    return left;
}

val_t parse_expr(ctx_t *c) {
    val_t left = parse_term(c);
    while (c->tok == T_PLUS || c->tok == T_MINUS) {
        int op = c->tok;
        next_tok(c);
        val_t right = parse_term(c);
        if (c->err_pos) return left;
        if (left.is_f || right.is_f) {
            double l = left.is_f ? left.f : (double)left.i;
            double r = right.is_f ? right.f : (double)right.i;
            left.is_f = 1;
            if (op == T_PLUS) left.f = l + r;
            else left.f = l - r;
        } else {
            if (op == T_PLUS) left.i += right.i;
            else left.i -= right.i;
        }
    }
    return left;}

void proc_file(const char *in_path, const char *out_path) {
    FILE *fin = fopen(in_path, "rb");
    if (!fin) { fprintf(stderr, "cannot open %s\n", in_path); exit(1); }
    
    fseek(fin, 0, SEEK_END);
    long size = ftell(fin);
    rewind(fin);
    
    char *buf = malloc(size + 2);
    if (!buf) { fclose(fin); exit(1); }
    fread(buf, 1, size, fin);
    buf[size] = '\0';
    fclose(fin);
    FILE *fout = fopen(out_path, "w");
    if (!fout) { fprintf(stderr, "cannot write %s\n", out_path); free(buf); exit(1); }
    
    char *p = buf;
    int first_err = 0;
    
    while (*p) {
        char *line_start = p;
        while (*p == ' ' || *p == '\t') p++;
        
        if (*p == '#') {
            while (*p && *p != '\n') p++;
            if (*p == '\n') p++;
            continue;}
        
        if (*p == '\n') {
            p++;
            continue;}
        
        if (*p == '\0') break;
        
        ctx_t c = { line_start, buf, 0, line_start, 0, {0, 0, 0.0} };
        next_tok(&c);
        
        val_t res = parse_expr(&c);
        
        if (c.tok != T_EOL && c.tok != T_EOF && c.err_pos == 0) {
            set_err(&c, c.tok_ptr);}
        
        if (c.err_pos) {
            first_err = c.err_pos;
            break;
        }
        if (res.is_f) fprintf(fout, "%.15g\n", res.f);
        else fprintf(fout, "%lld\n", res.i);
        
        p = c.txt;
        while (*p && *p != '\n') p++;
        if (*p == '\n') p++;}
    if (first_err) {
    freopen(out_path, "w", fout);
    fprintf(fout, "ERROR:%d\n", first_err);
    }
    
    fclose(fout);
    free(buf);
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "no args\n"); return 1; }
    
char *in_dir = NULL;
    char *out_dir = NULL;
    char *in_file = NULL;
    
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
        if (i + 1 < argc) in_dir = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
        if (i + 1 < argc) out_dir = argv[++i];
        } else if (argv[i][0] == '-') {
        fprintf(stderr, "unknown option\n"); return 1;
        } else {
            in_file = argv[i];}
    }
    
    char def_out[256] = {0};
    if (!out_dir) {
        char *base = in_dir ? in_dir : in_file;
        char *slash = strrchr(base, '/');
        if (slash) base = slash + 1;       
        char name_copy[256];
        strncpy(name_copy, base, 255);
        char *dot = strrchr(name_copy, '.');
        if (dot && strcmp(dot, ".txt") == 0) *dot = '\0';
        sprintf(def_out, "%s_huseyn_251ADB188", name_copy);
        out_dir = def_out;}
    struct stat st = {0};
    if (stat(out_dir, &st) == -1) {
        mkdir(out_dir, 0700);}
    if (in_dir) {
        DIR *d = opendir(in_dir);
        if (!d) { fprintf(stderr, "missing dir\n"); return 1; }
        struct dirent *dir;
        while ((dir = readdir(d)) != NULL) {
            int len = strlen(dir->d_name);
            if (len > 4 && strcmp(dir->d_name + len - 4, ".txt") == 0) {
                char ipath[512], opath[512];
                sprintf(ipath, "%s/%s", in_dir, dir->d_name);
                char base[256];
                strcpy(base, dir->d_name);
                base[len - 4] = '\0';
                sprintf(opath, "%s/%s_huseyn_tahirzade_251ADB188.txt", out_dir, base);
                proc_file(ipath, opath);
            }
        }
        closedir(d);
    } else if (in_file) {
        FILE *chk = fopen(in_file, "r");
        if (!chk) { fprintf(stderr, "missing file\n"); return 1; }
        fclose(chk);
        char *slash = strrchr(in_file, '/');
        char *base = slash ? slash + 1 : in_file;
        char name_copy[256];
        strncpy(name_copy, base, 255);
        char *dot = strrchr(name_copy, '.');
        if (dot) *dot = '\0';
        char opath[512];
        sprintf(opath, "%s/%s_huseyn_tahirzade_251ADB188.txt", out_dir, name_copy);
        proc_file(in_file, opath);
    } else {
        fprintf(stderr, "no input\n"); return 1;
    }
    return 0;
}