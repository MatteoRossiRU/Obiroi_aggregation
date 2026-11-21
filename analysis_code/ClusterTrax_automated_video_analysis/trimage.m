function [TrimmedImage, corner_x, corner_y, length_x, length_y] = ...
    trimage(Original_Image, square, edge_buffer, trimmed)
    
    arguments
       Original_Image
       square {mustBeMember(square,[0, 1])} = 1
       edge_buffer {mustBeNonnegative} = 25
       trimmed {mustBeMember(trimmed, [0, 1])} = 0
    end
    
    figure
    imshow(Original_Image)
    
    if trimmed == 0
        message = sprintf('Please select image bounds');
        questdlg(message, 'Instruction', 'Okay', 'Close', 'Okay');
        [x, y] = ginput(2);
        length_x = floor(abs(x(2) - x(1))) + edge_buffer * 2;
        if square == 1
            length_y = length_x;
        else
            length_y = floor(abs(y(2) - y(1))) + edge_buffer * 2;
        end
    else
        length_x = evalin('caller', 'length_x');
        length_y = evalin('caller', 'length_y');
    end
    
    message = sprintf('Please select top left corner');
    questdlg(message, 'Instruction', 'Okay', 'Close', 'Okay');
    
    [corner_x, corner_y] = ginput(1);
    corner_x = floor(corner_x) - edge_buffer;
    corner_y = floor(corner_y) - edge_buffer;
    
%     if ndims(Original_Image) == 3
    TrimmedImage = Original_Image(corner_y : corner_y + length_y,...
            corner_x : corner_x + length_x, :);
%     else
%         TrimmedImage = Original_Image(corner_y : corner_y + length_y,...
%             corner_x : corner_x + length_x);
%     end
    
    imshow(TrimmedImage);
    drawnow;
    message = sprintf('Is this Okay?');
    reply = questdlg(message);
    
    while reply ~= "Yes"
        if strcmpi(reply, 'Cancel')
            close gcf
            return;
        end
        if strcmpi(reply, '')
            close gcf
            return;
        end
        
        imshow(Original_Image);
        [corner_x, corner_y] = ginput(1);
        corner_x = floor(corner_x) - edge_buffer;
        corner_y = floor(corner_y) - edge_buffer;
        
        if ndims(Original_Image) == 3
            TrimmedImage = Original_Image(corner_y : corner_y + length_y,...
                corner_x : corner_x + length_x, :);
        else
            TrimmedImage = Original_Image(corner_y : corner_y + length_y,...
                corner_x : corner_x + length_x);
        end
        
        imshow(TrimmedImage);
        drawnow;
        message = sprintf('Is this Okay?');
        reply = questdlg(message);
    end
    
    close gcf
end