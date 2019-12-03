
#include <glm/glm.hpp>
#include <limits>

extern "C"
{
#include "escheme.h"
}

#ifdef RKT_MOD_NAME
const char* mod_name = RKT_MOD_NAME;
#else
const char* mod_name = "glm-shim";
#endif

template <size_t L>
struct DVec
{
  Scheme_Object so;
  glm::vec<L, double, glm::defaultp> data;
};

typedef DVec<2> DVec2;
typedef DVec<3> DVec3;
typedef DVec<3> DVec4;

#ifdef MZ_PRECISE_GC

template <size_t L>
static int dvec_size(void* p) {
  return gcBYTES_TO_WORDS(sizeof(DVec<L>));
}

template <size_t L>
static int dvec_mark(void* p) {
  gcMARK(((DVec<L>*) p)->data);
  return gcBYTES_TO_WORDS(sizeof(DVec<L>));
}

template <size_t L>
static int dvec_fixup(void* p){
  gcFIXUP(((DVec<L>*) p)->data);
  return gcBYTES_TO_WORDS(sizeof(DVec<L>));
}

#endif

static Scheme_Type dvec2_type, dvec3_type, dvec4_type;

template <size_t L>
Scheme_Object* make_dvec(int argc, Scheme_Object** argv)
{
  DVec<L>* result = (DVec<L>*) scheme_malloc_tagged(sizeof(DVec<L>));
  switch (L)
    {
    case 2: result->so.type = dvec2_type; break;
    case 3: result->so.type = dvec3_type; break;
    case 4: result->so.type = dvec4_type; break;
    }
  if (argc == 0)
    result->data = glm::vec<L, double, glm::defaultp>();
  else if (argc == 1)
    result->data = glm::vec<L, double, glm::defaultp>(SCHEME_DBL_VAL(argv[0]));
  else if (argc == L) {
    result->data = glm::vec<L, double, glm::defaultp>();
    for (int i=0; i < L; ++i)
      result->data[i] = SCHEME_DBL_VAL(argv[i]);
  }
  return (Scheme_Object*) result;
}

// Scheme_Object* dvec_len(int argc, Scheme_Object** argv)
// {
//   if (SCHEME_TYPE(argv[0]) == dvec2_type)
//     return scheme_make_integer(2);
//   if (SCHEME_TYPE(argv[0]) == dvec3_type)
//     return scheme_make_integer(3);
//   if (SCHEME_TYPE(argv[0]) == dvec4_type)
//     return scheme_make_integer(4);
// }

// template <size_t L>
// Scheme_Object* _dvec_lst(int argc, Scheme_Object** argv)
// {
//   int len = SCHEME_INT_VAL(dvec_len(argc, argv));
//   DVec<L>* v = (DVec<L>*) argv[0];
//   Scheme_Object* result = scheme_null;
//   for (int i=0; i < len; i++)
//     result =
//       scheme_append(result, scheme_make_pair(scheme_make_double(v->data[i]), scheme_null));
//   return result;
// }

// Scheme_Object* dvec_lst(int argc, Scheme_Object** argv)
// {
//   if (SCHEME_TYPE(argv[0]) == dvec2_type)
//     return _dvec_lst<2>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec3_type)
//     return _dvec_lst<3>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec4_type)
//     return _dvec_lst<4>(argc, argv);
// }

// template <size_t L>
// Scheme_Object* _dvec_ref(int argc, Scheme_Object** argv)
// {
//   DVec<L>* v = (DVec<L>*) argv[0];
//   int i = SCHEME_INT_VAL(argv[1]);
//   return scheme_make_double(v->data[i]);
// }

// Scheme_Object* dvec_ref(int argc, Scheme_Object** argv)
// {
//   if (SCHEME_TYPE(argv[0]) == dvec2_type)
//     return _dvec_ref<2>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec3_type)
//     return _dvec_ref<3>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec4_type)
//     return _dvec_ref<4>(argc, argv);
// }

// template <size_t L>
// Scheme_Object* _dvec_set(int argc, Scheme_Object** argv)
// {
//   DVec<L>* v = (DVec<L>*) argv[0];
//   int i = SCHEME_INT_VAL(argv[1]);
//   v->data[i] = SCHEME_DBL_VAL(argv[2]);
//   return scheme_void;
// }

// Scheme_Object* dvec_set(int argc, Scheme_Object** argv)
// {
//   if (SCHEME_TYPE(argv[0]) == dvec2_type)
//     return _dvec_set<2>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec3_type)
//     return _dvec_set<3>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec4_type)
//     return _dvec_set<4>(argc, argv);
// }

template <size_t L>
Scheme_Object* _dvec_mul(int argc, Scheme_Object** argv)
{
  DVec<L>* v1 = (DVec<L>*) argv[0];
  DVec<L>* v2 = (DVec<L>*) argv[1];
  DVec<L>* result = (DVec<L>*) make_dvec<L>(0, NULL);
  result->data = v1->data * v2->data;
  return (Scheme_Object*) result;
}

Scheme_Object* dvec_mul(int argc, Scheme_Object** argv)
{
  if (SCHEME_TYPE(argv[0]) == dvec2_type)
    return _dvec_mul<2>(argc, argv);
  if (SCHEME_TYPE(argv[0]) == dvec3_type)
    return _dvec_mul<3>(argc, argv);
  if (SCHEME_TYPE(argv[0]) == dvec4_type)
    return _dvec_mul<4>(argc, argv);
}

// template <size_t L>
// Scheme_Object* _dvec_mul_bx(int argc, Scheme_Object** argv) {
//   DVec<L>* v1 = (DVec<L>*) argv[0];
//   DVec<L>* v2 = (DVec<L>*) argv[1];
//   v1->data *= v2->data;
//   return (Scheme_Object*) v1;
// }

// Scheme_Object* dvec_mul_bx(int argc, Scheme_Object** argv) {
//   if (SCHEME_TYPE(argv[0]) == dvec2_type)
//     return _dvec_mul_bx<2>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec3_type)
//     return _dvec_mul_bx<3>(argc, argv);
//   if (SCHEME_TYPE(argv[0]) == dvec4_type)
//     return _dvec_mul_bx<4>(argc, argv);
// }

Scheme_Object* scheme_reload(Scheme_Env *env) {
  return scheme_initialize(env);
}

Scheme_Object* scheme_initialize(Scheme_Env* env) {
  dvec2_type = scheme_make_type("<dvec2>");
  dvec3_type = scheme_make_type("<dvec3>");
  dvec4_type = scheme_make_type("<dvec4>");

  #ifdef MZ_PRECISE_GC

  GC_register_traversers(dvec2_type, dvec_size<2>, dvec_mark<2>, dvec_fixup<2>, 1, 0);
  GC_register_traversers(dvec3_type, dvec_size<3>, dvec_mark<3>, dvec_fixup<3>, 1, 0);
  GC_register_traversers(dvec4_type, dvec_size<4>, dvec_mark<4>, dvec_fixup<4>, 1, 0);

  #endif

  Scheme_Env* mod_env = scheme_primitive_module(scheme_intern_symbol(mod_name), env);

  Scheme_Object* dvec2_prim = scheme_make_prim_w_arity(make_dvec<2>, "dvec2", 0, 2);
  Scheme_Object* dvec3_prim = scheme_make_prim_w_arity(make_dvec<3>, "dvec3", 0, 3);
  Scheme_Object* dvec4_prim = scheme_make_prim_w_arity(make_dvec<4>, "dvec4", 0, 4);
  // Scheme_Object* dvec_len_prim = scheme_make_prim_w_arity(dvec_len, "dvec-length", 1, 1);
  // Scheme_Object* dvec_lst_prim = scheme_make_prim_w_arity(dvec_lst, "dvec->list", 1, 1);
  // Scheme_Object* dvec_ref_prim = scheme_make_prim_w_arity(dvec_ref, "dvec-ref", 2, 2);
  // Scheme_Object* dvec_set_prim = scheme_make_prim_w_arity(dvec_set, "dvec-set!", 3, 3);
  Scheme_Object* dvec_mul_prim = scheme_make_prim_w_arity(dvec_mul, "dvec*", 2, 2);
  Scheme_Object* dvec2_mul_prim = scheme_make_prim_w_arity(_dvec_mul<2>, "dvec2*", 2, 2);
  Scheme_Object* dvec3_mul_prim = scheme_make_prim_w_arity(_dvec_mul<3>, "dvec3*", 2, 2);
  Scheme_Object* dvec4_mul_prim = scheme_make_prim_w_arity(_dvec_mul<4>, "dvec4*", 2, 2);
  // Scheme_Object* dvec_mul_bx_prim = scheme_make_prim_w_arity(dvec_mul_bx, "dvec*=!", 2, 2);

  scheme_add_global("dvec2", dvec2_prim, mod_env);
  scheme_add_global("dvec3", dvec3_prim, mod_env);
  scheme_add_global("dvec4", dvec4_prim, mod_env);
  // scheme_add_global("dvec-length", dvec_len_prim, mod_env);
  // scheme_add_global("dvec->list", dvec_lst_prim, mod_env);
  // scheme_add_global("dvec-ref", dvec_ref_prim, mod_env);
  // scheme_add_global("dvec-set!", dvec_set_prim, mod_env);
  scheme_add_global("dvec*", dvec_mul_prim, mod_env);
  scheme_add_global("dvec2*", dvec2_mul_prim, mod_env);
  scheme_add_global("dvec3*", dvec3_mul_prim, mod_env);
  scheme_add_global("dvec4*", dvec4_mul_prim, mod_env);
  // scheme_add_global("dvec*=!", dvec_mul_bx_prim, mod_env);

  scheme_finish_primitive_module(mod_env);

  return scheme_void;
}

Scheme_Object* scheme_module_name() {
  return scheme_intern_symbol(mod_name);
}
