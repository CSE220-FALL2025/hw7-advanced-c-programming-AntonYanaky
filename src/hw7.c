#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        bst_sf *newNode = (bst_sf*)malloc(sizeof(bst_sf));
        if (!newNode) return NULL;
        newNode->mat = mat;
        newNode->left_child = NULL;
        newNode->right_child = NULL;
        return newNode;
    }
    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else if (mat->name > root->mat->name) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) return NULL;

    if (name == root->mat->name) {
        return root->mat;
    } else if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) return;
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (!mat1 || !mat2) return NULL;
    if (mat1->num_rows != mat2->num_rows || mat1->num_cols != mat2->num_cols) return NULL;
    
    unsigned int rows = mat1->num_rows;
    unsigned int cols = mat1->num_cols;
    unsigned int size = rows * cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + size * sizeof(int));
    if (!result) return NULL;
    
    result->name = '!';
    result->num_rows = rows;
    result->num_cols = cols;
    
    for (unsigned int i = 0; i < size; i++) {
        result->values[i] = mat1->values[i] + mat2->values[i];
    }
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (!mat1 || !mat2) return NULL;
    if (mat1->num_cols != mat2->num_rows) return NULL;
    
    unsigned int m = mat1->num_rows;
    unsigned int n = mat1->num_cols;
    unsigned int p = mat2->num_cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + m * p * sizeof(int));
    if (!result) return NULL;
    
    result->name = '!';
    result->num_rows = m;
    result->num_cols = p;
    
    for (unsigned int i = 0; i < m; i++) {
        for (unsigned int j = 0; j < p; j++) {
            int sum = 0;
            for (unsigned int k = 0; k < n; k++) {
                sum += mat1->values[i * n + k] * mat2->values[k * p + j];
            }
            result->values[i * p + j] = sum;
        }
    }
    return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    if (!mat) return NULL;
    
    unsigned int r = mat->num_rows;
    unsigned int c = mat->num_cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + c * r * sizeof(int));
    if (!result) return NULL;
    
    result->name = '!';
    result->num_rows = c;
    result->num_cols = r;
    
    for (unsigned int i = 0; i < r; i++) {
        for (unsigned int j = 0; j < c; j++) {
            result->values[j * r + i] = mat->values[i * c + j];
        }
    }
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    if (!expr) return NULL;
    const char *p = expr;
    
    while (*p && isspace(*p)) p++;
    unsigned int rows = strtoul(p, (char**)&p, 10);
    
    while (*p && isspace(*p)) p++;
    unsigned int cols = strtoul(p, (char**)&p, 10);
    
    matrix_sf *m = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    if (!m) return NULL;
    
    m->name = name;
    m->num_rows = rows;
    m->num_cols = cols;
    
    while (*p && *p != '[') p++;
    if (*p == '[') p++;
    
    for (unsigned int i = 0; i < rows * cols; i++) {
        while (*p && (isspace(*p) || *p == ';')) p++;
        if (!*p || *p == ']') break;
        m->values[i] = (int)strtol(p, (char**)&p, 10);
    }
    return m;
}

int precedence(char op) {
    if (op == '+') return 1;
    if (op == '*') return 2;
    if (op == '\'') return 3;
    return 0;
}

char* infix2postfix_sf(char *infix) {
    if (!infix) return NULL;
    int len = strlen(infix);
    char *postfix = malloc(len * 2 + 1);
    char *stack = malloc(len + 1);
    if (!postfix || !stack) { free(postfix); free(stack); return NULL; }
    
    int top = -1, j = 0;
    
    for (int i = 0; i < len; i++) {
        char c = infix[i];
        if (isspace(c)) continue;
        
        if (isalpha(c)) {
            postfix[j++] = c;
        } else if (c == '(') {
            stack[++top] = c;
        } else if (c == ')') {
            while (top >= 0 && stack[top] != '(') {
                postfix[j++] = stack[top--];
            }
            if (top >= 0) top--;
        } else {
            while (top >= 0 && stack[top] != '(' &&
                   (precedence(stack[top]) > precedence(c) ||
                    (precedence(stack[top]) == precedence(c) && c != '\''))) {
                postfix[j++] = stack[top--];
            }
            stack[++top] = c;
        }
    }
    
    while (top >= 0) postfix[j++] = stack[top--];
    postfix[j] = '\0';
    free(stack);
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    if (!postfix) return NULL;
    
    int len = strlen(postfix);
    matrix_sf **stack = malloc(len * sizeof(matrix_sf*));
    if (!stack) { free(postfix); return NULL; }
    int top = -1;
    
    for (int i = 0; i < len; i++) {
        char c = postfix[i];
        
        if (isalpha(c)) {
            matrix_sf *m = find_bst_sf(c, root);
            if (!m) {
                while (top >= 0) {
                    if (stack[top] && !isalpha(stack[top]->name)) free(stack[top]);
                    top--;
                }
                free(stack);
                free(postfix);
                return NULL;
            }
            stack[++top] = m;
        } else if (c == '\'') {
            if (top < 0) { free(stack); free(postfix); return NULL; }
            matrix_sf *mat = stack[top--];
            matrix_sf *res = transpose_mat_sf(mat);
            if (mat && !isalpha(mat->name)) free(mat);
            if (!res) { free(stack); free(postfix); return NULL; }
            stack[++top] = res;
        } else {
            if (top < 1) { free(stack); free(postfix); return NULL; }
            matrix_sf *m2 = stack[top--];
            matrix_sf *m1 = stack[top--];
            matrix_sf *res = (c == '+') ? add_mats_sf(m1, m2) : mult_mats_sf(m1, m2);
            if (m1 && !isalpha(m1->name)) free(m1);
            if (m2 && !isalpha(m2->name)) free(m2);
            if (!res) { free(stack); free(postfix); return NULL; }
            stack[++top] = res;
        }
    }
    
    matrix_sf *result = (top >= 0) ? stack[top--] : NULL;
    if (result) result->name = name;
    
    while (top >= 0) {
        if (stack[top] && !isalpha(stack[top]->name)) free(stack[top]);
        top--;
    }
    free(stack);
    free(postfix);
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) return NULL;
    
    char *line = NULL;
    size_t n = 0;
    bst_sf *root = NULL;
    matrix_sf *last = NULL;
    
    while (getline(&line, &n, file) != -1) {
        char *end = line + strlen(line);
        while (end > line && (end[-1] == '\n' || end[-1] == '\r' || end[-1] == ' ' || end[-1] == '\t')) {
            end--;
        }
        *end = '\0';
        
        char *p = line;
        while (*p && isspace(*p)) p++;
        if (!*p || *p == '#') continue;
        if (!isalpha(*p)) continue;
        
        char name = *p;
        char *eq = strchr(p, '=');
        if (!eq) continue;
        
        eq++;
        while (*eq && isspace(*eq)) eq++;
        
        end = eq + strlen(eq);
        while (end > eq && (end[-1] == '\n' || end[-1] == '\r' || end[-1] == ' ' || end[-1] == '\t')) {
            end--;
        }
        *end = '\0';
        
        matrix_sf *mat = strchr(eq, '[') ? create_matrix_sf(name, eq) : evaluate_expr_sf(name, eq, root);
        if (mat) {
            root = insert_bst_sf(mat, root);
            last = mat;
        }
    }
    
    fclose(file);
    if (line) free(line);
    
    matrix_sf *result = NULL;
    if (last) {
        result = copy_matrix(last->num_rows, last->num_cols, last->values);
        if (result) result->name = last->name;
    }
    free_bst_sf(root);
    return result;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\r\n");
}