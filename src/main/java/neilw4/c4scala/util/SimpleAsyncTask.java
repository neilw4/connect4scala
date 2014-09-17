package neilw4.c4scala.util;

import android.os.AsyncTask;

/**
 * Adapted from https://www.assembla.com/code/scala-eclipse-toolchain/git/nodes/ad17dd4047ac25b167496db84f4272795eb93c3e/docs/android-examples/android-sdk/Wiktionary/src/com/example/android/wiktionary/SimpleAsyncTask.java
 * Temporary workaround to solve a Scala compiler issue which shows up
 * at runtime with the error message
 * "java.lang.AbstractMethodError: abstract method not implemented"
 * for the missing method LookupTask.doInBackground(V... args).
 *
 * Our solution: the Java method doInBackground(V... args) forwards
 * the call to the Scala method doInBackground().
 */
public abstract class SimpleAsyncTask<U, V> extends AsyncTask<U, Void, V> {

    protected abstract V doInBackground(U arg);

    @Override
    protected V doInBackground(U... args) {
        return doInBackground(args.length > 0 ? args[0]: null);
    }

}
