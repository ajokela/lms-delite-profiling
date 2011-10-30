#include <jni.h>
#include "ppl_dsl_deliszt_datastruct_scala_MeshLoader.h"
#include "MeshLoader.h"

System::MeshLoader ml;

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader_loadMesh (JNIEnv* env, jobject obj, jstring str) {
  std::cout << "init" << std::endl;
  ml.init(env);

  std::cout << "load" << std::endl;
  jobject jmesh = ml.loadMesh(str);

  if(jmesh)  
    std::cout << "has mesh" << std::endl;
  else
    std::cout << "no mesh" << std::endl;

  return jmesh;
}

JNIEXPORT jobject JNICALL Java_generated_scala_MeshLoader_loadMesh (JNIEnv* env, jobject obj, jstring str) {
  std::cout << "init" << std::endl;
  ml.init(env, true);

  std::cout << "load" << std::endl;
  jobject jmesh = ml.loadMesh(str);

  if(jmesh)  
    std::cout << "has mesh" << std::endl;
  else
    std::cout << "no mesh" << std::endl;

  return jmesh;
}

JNIEXPORT jobject JNICALL Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader__1loadBoundarySet
  (JNIEnv * env, jobject obj, jstring str, jint type) {
  string name(env->GetStringUTFChars(str, 0));
  
  if(type == 0) {
    return ml.loadBoundarySet<LisztPrivate::ElemTypes::VertexType>(name.c_str());
  }
  else if(type == 1) {
    return ml.loadBoundarySet<LisztPrivate::ElemTypes::EdgeType>(name.c_str());
  }
  else if(type == 2) {
    return ml.loadBoundarySet<LisztPrivate::ElemTypes::FaceType>(name.c_str());
  }
  else if(type == 3) {
    return ml.loadBoundarySet<LisztPrivate::ElemTypes::CellType>(name.c_str());
  }
  
  return NULL;
}

JNIEXPORT jobject JNICALL Java_generated_scala_MeshLoader__1loadBoundarySet
  (JNIEnv * env, jobject obj, jstring str, jint type) {
  return Java_ppl_dsl_deliszt_datastruct_scala_MeshLoader__1loadBoundarySet(env, obj, str, type);
}
