package spaken.model;

public class NullVectorException extends Exception {
  private static final String MESSAGE = "Attempt to do something naughty with null vector";
  
  public NullVectorException() {
    super(MESSAGE);
  }
  
}
