#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <limits.h>
#include <float.h>

typedef 
    struct input{
        char letter;
        float lower;
        float upper;
        float gap;
        float *prob;
        float *low_limits;
        int *freq;
        int sum;
    } input;

typedef
    struct final{
        double num;
        char var;
        struct final *next;
    } sfinal;

/*global variables*/ 
sfinal *final;
sfinal **last_node=&final;
char stack[50];
char *last_stack=stack;

/*input variables*/
char *formula;
int inter;
long int expe;
input *vars;
/*input variables*/

int main(){
    void func_replace(char *func);
    void compactify_string(char *s);
    void func_replace(char *func);
    int inter_det(int i, int *freq);
    void place(char c, double num);
    void shunting_yard(char *formula);
    double randomize(double low, double up);
    void freefinal(sfinal *s);
    void freeinput(input *var, int var_num);
    double evaluate(sfinal *final);
    /*temporary variables*/
    input *inp;
    float *f;
    int *a;
    char *s, *k;
    int i, j;
    int var_num=0;
    char arr[30];
    double limit;
    double temp;
    long int li;
    
    /*variables for output*/
    int rand_inter;
    int x;
    double rand_num;
    double *result;
    double *r, *d;
    double max_value=-DBL_MAX;
    double min_value=DBL_MAX;
    double output_gap;
    double *out_posib;
    double *up_lim;

    /*input*/
    formula=malloc(200*sizeof(char));
    scanf("%[^\n]%*c", formula);
    scanf("%d %ld", &inter, &expe);
    /* variable number determination*/
    for (k=arr,s=formula;*s;s++){
        if ((*s)>=65&&(*s)<=90){
            var_num++;
            *k=*s;
            k++;
        }
    }
    *k=0;
    for (s=arr;*(s+1);s++){
        for (k=s+1;*k;k++){
            if (*s==*k){
                *k=' ';
                compactify_string(s);
                var_num--;
                k--;
            }
        }
    }
    vars=malloc(sizeof(input)*var_num);
    /*variable number determination*/

    for (i=0;i<var_num;i++){
        ((vars+i)->prob)=malloc(sizeof(float)*inter);
        scanf(" %c %f %f", &((vars+i)->letter), &((vars+i)->lower), &((vars+i)->upper));
        for (j=0,f=((vars+i)->prob);j<inter;j++,f++){
            scanf("%f", (((vars+i)->prob)+j));
        }
    }
    /*input end*/
    compactify_string(formula);
    func_replace(formula);

    /*infix to postfix*/
    shunting_yard(formula);
    /*infix to postfix*/
    free(formula);

    for (i=0,inp=vars;i<var_num;i++,inp++){
        (inp->gap)=((inp->upper)-(inp->lower))/(float)inter; /*interval gap*/
        (inp->low_limits)=malloc(sizeof(float)*inter);
        (inp->freq)=malloc(sizeof(int)*inter);
        for (j=0,f=(inp->low_limits);j<inter;j++,f++){  /*lower limits determination*/
            *f=(inp->lower)+(j*(inp->gap));
        }
        for (j=0,a=(inp->freq),f=(inp->prob);j<inter;f++,j++,a++){ /*frequency of intervals*/
            *a=(*f)*1000;
        }
        for (j=1,a=((inp->freq)+1);j<inter;a++,j++){ /*for randomize algorithm*/
            (*a)=(*a)+(*(a-1));
        }
        inp->sum=*(a-1); /*for randomize algorithm*/
    }

    srand(time(NULL));
    result = malloc(sizeof(double)*expe);
    for (li=0;li<expe;li++){
        for (j=0;j<var_num;j++){
            x=1+rand()%(vars[j].sum);
            rand_inter=inter_det(x, vars[j].freq);
            limit=(vars[j].low_limits)[rand_inter];
            rand_num=randomize(limit,limit+vars[j].gap);
            place(vars[j].letter, rand_num);
        }
        temp=evaluate(final);
        result[li]=temp;
        if (temp>max_value){
            max_value=temp;
        }
        if (temp<min_value){
            min_value=temp;
        }
    }
    freefinal(final);
    for (i=0;i<var_num;i++){
        free((vars+i)->prob);
        free((vars+i)->freq);
        free((vars+i)->low_limits);
    }
    free(vars);
    
    output_gap=(max_value-min_value)/(double)inter;
    up_lim=malloc(sizeof(double)*inter);
    out_posib=malloc(sizeof(double)*inter);
    for (i=0;i<inter;i++){
        out_posib[i]=0;
    }
    for (i=1,r=up_lim;i<=inter;i++,r++){
        *r=min_value+(i)*output_gap;
    }

    for (li=0,r=result;li<expe;r++,li++){
        for (j=0,d=up_lim;j<inter;d++,j++){
            if (*r<=*d){
                (out_posib[j])++;
                break;
            }
        }
    }
    for (i=0,r=out_posib;i<inter;r++,i++){
        (*r)=(*r)/(double)expe;
    }
    printf("%.3f %.3f", min_value, max_value);
    for (i=0,r=out_posib;i<inter;r++,i++){
        printf(" %.3f", *r);
    }
    printf("\n");

    free(result);
    free(up_lim);
    free(out_posib);

return 0;
}

void shunting_yard(char *formula){
    int precedence(char s);
    int isOperand(char s);
    int isOperator(char s);
    int isFunction(char s);
    int isDigit(char c);
    char *s;
    char *k;
    for(s=formula;*s;s++){
        if (*s=='('){
            *last_stack='(';
            last_stack++;
            *last_stack=0;

        }
        else if (*s==')'){
            for (k=last_stack-1;(*k)!='(';k--){
                *last_node=malloc(sizeof(sfinal));
                (*last_node)->var=*k;
                *k=0;
                (*last_node)->num=0;
                (*last_node)->next=NULL;
                last_node=&((*last_node)->next);
            }
            *k=0;
            last_stack=k;
        }
        else if (isFunction(*s)){
            *last_stack=*s;
            last_stack++;
            *last_stack=0;
        }
        else if (isOperand(*s)){
            *last_node=malloc(sizeof(sfinal));
            (*last_node)->var=*s;
            (*last_node)->num=0;
            (*last_node)->next=NULL;
            last_node=&((*last_node)->next);
        }
        else if (isOperator(*s)){
            while(precedence(*s)){
                k=(last_stack-1);
                *last_node=malloc(sizeof(sfinal));
                (*last_node)->var=*k;
                *k=0;
                (*last_node)->num=0;
                (*last_node)->next=NULL;
                last_node=&((*last_node)->next);
                last_stack--;
            }
            *last_stack=*s;
            last_stack++;
            *last_stack=0;
        }
        else if (isDigit(*s)){
            (*last_node)=malloc(sizeof(sfinal));
            sscanf(s, "%lf", &((*last_node)->num));
            (*last_node)->var=0;
            (*last_node)->next=NULL;
            last_node=&((*last_node)->next);
            while (isDigit(*s)||(*s=='.')){
                s++;
            }
            s--;
        }
    }
    if (last_stack!=stack){
        for (k=last_stack-1;stack!=last_stack;k--){
            *last_node=malloc(sizeof(sfinal));
            (*last_node)->var=*k;
            (*last_node)->num=0;
            (*last_node)->next=NULL;
            last_node=&((*last_node)->next);
            *k=0;
            last_stack--;
        }
    }
}

double evaluate(sfinal *final){   
    int length(sfinal *s);
    int isOperator(char s);
    int isOperand(char s);
    int isFunction(char s);
    sfinal *s;
    char c;
    double *stack;
    double *last_stack;
    double result;
    stack=malloc(length(final)*sizeof(double));
    last_stack=stack;
    for (s=final;s;s=s->next){
        if ((s->var)!=0){
            c=s->var;
            if (isOperator(c)){
                if (c=='+'){
                    *(last_stack-2)=(*(last_stack-2))+(*(last_stack-1));
                    last_stack--;
                    *last_stack=0;
                }
                else if (c=='-'){
                    *(last_stack-2)=(*(last_stack-2))-(*(last_stack-1));
                    last_stack--;
                    *last_stack=0;
                }
                else if (c=='*'){
                    *(last_stack-2)=(*(last_stack-2))*(*(last_stack-1));
                    last_stack--;
                    *last_stack=0;
                }
                else if (c=='/'){
                    *(last_stack-2)=(*(last_stack-2))/(*(last_stack-1));
                    last_stack--;
                    *last_stack=0;
                }
                else if (c=='^'){
                    *(last_stack-2)=pow(*(last_stack-2),*(last_stack-1));
                    last_stack--;
                    *last_stack=0;
                }
            }
            else if (isFunction(c)){
                if (c=='s'){
                    *(last_stack-1)=sin(*(last_stack-1));
                }
                else if (c=='c'){
                    *(last_stack-1)=cos(*(last_stack-1));
                }
                else if (c=='#'){
                    *(last_stack-1)=sqrt(*(last_stack-1));
                }
                else if (c=='l'){
                    *(last_stack-1)=log(*(last_stack-1));
                }
                else if (c=='~'){
                    *(last_stack-1)=(-1)*(*(last_stack-1));
                }
            }
            else if (isOperand(c)){
                *last_stack=(s->num);
                last_stack++;
            }
        }
        else{
            /*sayı olması*/
            *last_stack=(s->num);
            last_stack++;
        }
    }
    result=*stack;
    free(stack);
    return result;
}


int length(sfinal *s){
    int i=1;
    while (s->next){
        i++;
        s=(s->next);
    }
    return i;
}

int isOperand(char s){
    if (s>=65&&s<=90){
        return 1;}
    else{
        return 0;}
}

int isOperator(char s){
    if ((s=='+')||(s=='-')||(s=='*')||(s=='/')||(s=='^')){
        return 1;}
    else {
        return 0;}
}

int isFunction(char s){
    if ((s=='~')||(s=='s')||(s=='c')||(s=='#')||(s=='l')){
        return 1;}
    else{
        return 0;}
}

int isDigit(char c){
    if (c==46||(c>=48&&c<=57)){
        return 1;
    }
    else {
        return 0;
    }
}

void compactify_string(char *s){
    char *p;
    for (p=s;*s;s++){
        if (*s==' '){
            continue;}
        else{
            if (s!=p){
                *p++=*s;}
            else{
                p++;}           
        }
    }
    *p=0;
}

void func_replace(char *func){
    void compactify_string(char *s);
    char *s;
    for (s=func;*s;s++){
        if ((*s=='s')&&(*(s+1)=='i')){
            s++;
            *s++=' ';
            *s=' ';
            compactify_string(func);
            s--; /*here*/
        }
        else if (*s=='c'){
            s++;
            *s++=' ';
            *s=' ';
            compactify_string(func);
            s--;/*here*/
        }
        else if ((*s=='s')&&(*(s+1)=='q')){
            *s++='#';
            *s++=' ';
            *s++=' ';
            *s=' ';
            compactify_string(func);
            s--;
            s--;/*here*/
        }
        else if (*s=='l'){
            s++;
            *s=' ';
            compactify_string(func);
        }
    }
}

int precedence(char s){
    int isFunction(char s);
    char last;
    if (last_stack!=stack){
        last=*(last_stack-1);
        if (last=='('){
            return 0;
        }
        else if ((s=='+')||(s=='-')){
            return 1;
        }
        else if ((s=='*')||(s=='/')){
            if ((last=='^')||(isFunction(last))||(last=='*')||(last=='/')){
                return 1;
            }
            else{
                return 0;
            }
        }
        else if (s=='^'){
            if (isFunction(last)){
                return 1;
            }
            else{
                return 0;
            }
        }
    }
    else {
        return 0;
    }
}

int inter_det(int i, int *freq){
    int j;
    for (j=0;;j++){
        if (i<=freq[j]){
            return j;
        }
    }
}

void place(char c, double num){
    sfinal *s;
    for (s=final;s;s=s->next){
        if ((s->var)==c){
            s->num=num;
        }
    }
}

double randomize(double low, double up){
    double result;
    result=(rand()%RAND_MAX)/(double)(RAND_MAX);
    result=result*(up-low)+low;
    return result;
}

void freefinal(sfinal *s){
    if (!(s->next)){
        free(s);
    }
    else{
        freefinal(s->next);
        free(s);
    }
}