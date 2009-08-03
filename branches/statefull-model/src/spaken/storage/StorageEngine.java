package spaken.storage;

import java.io.IOException;

import spaken.model.Space;

public interface StorageEngine {
	public void storeSpace(Space space) throws IOException;
	public void restoreSpace(Space space) throws IOException;
}
