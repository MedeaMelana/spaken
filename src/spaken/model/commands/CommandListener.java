package spaken.model.commands;


public interface CommandListener {

	public void commandExecuted(Command c);

	public void commandUndone(Command c);

	public void commandRedone(Command c);

}
